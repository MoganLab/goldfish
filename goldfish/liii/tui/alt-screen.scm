;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;; 备用屏（alt screen）驱动：整帧渲染的终端表面。
;; 与 (liii tui main-screen) 的区别：
;;   - main-screen 面向 inline 场景，内容行数可以超过终端高度，由终端自身滚动；
;;   - alt-screen 面向全屏场景，每帧固定为终端高度，行内容超出部分由调用方
;;     （滚动视口）裁剪，渲染时按绝对行号定位并只重绘变化的行。
(define-library (liii tui alt-screen)
  (export make-alt-screen alt-screen? alt-screen-terminal alt-screen-lines
    alt-screen-render! alt-screen-normalize alt-screen-clear!
    alt-screen-invalidate! alt-screen-refresh!
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii list)
    (liii string)
    (liii tui text-width)
    (liii tui terminal)
    (liii tui main-screen buffer)
    (liii tui main-screen diff)
  ) ;import
  (begin

    (define-record-type <alt-screen>
      (%make-alt-screen terminal buffer)
      alt-screen?
      (terminal %alt-screen-terminal)
      (buffer %alt-screen-buffer)
    ) ;define-record-type

    (define (%check-alt-screen screen caller)
      (unless (alt-screen? screen)
        (type-error (string-append caller
                      ": expected an <alt-screen>, got "
                      (object->string screen)
                    ) ;string-append
        ) ;type-error
      ) ;unless
    ) ;define

    (define (make-alt-screen term)
      (unless (terminal? term)
        (type-error (string-append "make-alt-screen: expected terminal, got " (object->string term))
        ) ;type-error
      ) ;unless
      (%make-alt-screen term (make-screen-buffer))
    ) ;define

    (define (alt-screen-terminal screen)
      (%check-alt-screen screen "alt-screen-terminal")
      (%alt-screen-terminal screen)
    ) ;define

    ;; 最近一次渲染的整帧内容（已按终端高度补齐/裁剪）
    (define (alt-screen-lines screen)
      (%check-alt-screen screen "alt-screen-lines")
      (screen-buffer-lines (%alt-screen-buffer screen))
    ) ;define

    ;; 整帧规范化：按终端宽度裁剪每行，并补齐到终端高度
    (define (%alt-frame term lines)
      (let* ((size (terminal-size term))
             (w (car size))
             (h (cadr size))
             (normalized (screen-normalize-lines lines "alt-screen-render!"))
            ) ;
        (let loop
          ((rest normalized) (acc '()) (i 0))
          (cond
           ((or (>= i h) (null? rest))
            (append (reverse acc) (make-list (- h (length acc)) ""))
           ) ;
           (else
             (loop (cdr rest) (cons (truncate-to-width (car rest) w "") acc) (+ i 1))
           ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (%render-op! term op w)
      (let ((type (diff-op-type op)) (idx (diff-op-index op)))
        (cond ((eq? type 'keep) #t)
              ((or (eq? type 'update) (eq? type 'append))
               (terminal-set-cursor! term 0 idx)
               ;; 先清行尾残留再写内容（对齐上游 ESC[2K 前置模式）：
               ;; 写满整行后终端处于 autowrap 待换行状态，其后紧跟清行
               ;; 序列会让 Windows conpty 丢弃末列字符（边框/页脚末字符消失）
               (terminal-clear-line-to-end term)
               (terminal-write term (diff-op-text op))
              ) ;
              ((eq? type 'clear) (terminal-set-cursor! term 0 idx) (terminal-clear-line term))
              (else #t)
        ) ;cond
      ) ;let
    ) ;define

    ;; 渲染一整帧：lines 为按行排列的帧内容，可选光标位置 '(x . y)
    ;; 光标定位与可见性切换必须纳入 sync 块（对齐上游 tui-alt-screen.ts
    ;; doRender 的 BEGIN..END 顺序）：行更新会把物理光标推到行首，
    ;; 定位序列若落在 sync 块外，IME pre-edit（跟随硬件光标）会闪烁。
    ;; 对外暴露帧归一化：pi-app-render! 用它缓存归一化帧，
    ;; 保证 Working 动画轻量 tick 构造的新帧与 buffer 上帧同源（逐行 eq? 命中）。
    (define (alt-screen-normalize screen lines)
      (%check-alt-screen screen "alt-screen-normalize")
      (%alt-frame (%alt-screen-terminal screen) lines)
    ) ;define

    ;; 至多一行变化检测：返回变化行号列表（按帧序），或 #f 表示不适用快速路径
    ;; （行数不一致或超过一行变化）。逐行先 eq?（共享对象），再 string=? 兜底。
    (define (%single-line-changes old-lines new-lines)
      (let loop
        ((a old-lines) (b new-lines) (y 0) (changed '()))
        (cond
         ((and (null? a) (null? b)) (if (<= (length changed) 1) (reverse changed) #f))
         ((or (null? a) (null? b)) #f)
         ((eq? (car a) (car b)) (loop (cdr a) (cdr b) (+ y 1) changed))
         ((string=? (car a) (car b)) (loop (cdr a) (cdr b) (+ y 1) changed))
         (else (loop (cdr a) (cdr b) (+ y 1) (cons y changed)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 硬件光标位置是否与请求位置一致
    (define (%cursor-same? term cur)
      (and cur
        (= (car cur) (car (terminal-cursor term)))
        (= (cdr cur) (cadr (terminal-cursor term)))
      ) ;and
    ) ;define

    ;; 光标定位/可见性是否需要输出：位置变化、可见性翻转或行更新
    ;; 推过光标时才发序列（行更新会移动物理光标，state 随之失效）
    (define (%cursor-output? term cur has-changes?)
      (or has-changes?
        (and cur
          (or
            (not (= (car cur) (car (terminal-cursor term))))
            (not (= (cdr cur) (cadr (terminal-cursor term))))
          ) ;or
        ) ;and
        (and (not cur) (terminal-cursor-visible? term))
      ) ;or
    ) ;define

    ;; 单行变化快速路径：归一化该行并输出，其余行保持；
    ;; commit 归一化后的完整帧（与原路径一致，保证下次比较同源）
    (define (%render-changed-line! screen buf term cur changes old-lines lines)
      (let* ((w (car (terminal-size term)))
             (y (car changes))
             (new-line (truncate-to-width (list-ref lines y) w ""))
             (new-frame (append (take old-lines y) (list new-line) (drop old-lines (+ y 1))))
            ) ;
        (terminal-begin-sync! term)
        (terminal-set-cursor! term 0 y)
        (terminal-clear-line-to-end term)
        (terminal-write term new-line)
        (screen-buffer-commit! buf new-frame)
        ;; 写入该行会把物理光标推到行尾，必须显式复位到请求位置
        ;; （否则光标会停在被改写行的末尾，输入区光标定位失效）
        (when cur
          (terminal-set-cursor! term (car cur) (cdr cur))
        ) ;when
        (when (terminal-cursor-visible? term)
          (terminal-hide-cursor! term)
        ) ;when
        (terminal-end-sync! term)
        (terminal-flush term)
        #t
      ) ;let*
    ) ;define

    ;; 原路径：全帧归一化 + 字符串 diff
    (define (%render-full-frame! screen buf term cur lines)
      (let* ((frame (%alt-frame term lines))
             (old-lines (screen-buffer-lines buf))
             (dirty? (screen-buffer-dirty? buf))
             (diff-ops (if dirty? (screen-dirty-diff old-lines frame) (screen-diff old-lines frame))
             ) ;diff-ops
             (w (car (terminal-size term)))
             (has-changes? (screen-diff-has-changes? diff-ops))
             (cursor-output? (%cursor-output? term cur has-changes?))
            ) ;
        (when (or has-changes? cursor-output?)
          (terminal-begin-sync! term)
        ) ;when
        (when has-changes?
          (for-each (lambda (op) (%render-op! term op w)) diff-ops)
        ) ;when
        (screen-buffer-commit! buf frame)
        ;; 光标定位但保持隐藏（对齐上游 tui-alt-screen.ts：
        ;; 硬件光标仅用于 IME 候选窗定位，输入区光标由软件反显方块绘制，
        ;; 视觉形状不随终端默认光标（Windows beam / Linux block）变化）
        (when cursor-output?
          (when cur
            (terminal-set-cursor! term (car cur) (cdr cur))
          ) ;when
          (when (terminal-cursor-visible? term)
            (terminal-hide-cursor! term)
          ) ;when
        ) ;when
        (when (or has-changes? cursor-output?)
          (terminal-end-sync! term)
        ) ;when
        (terminal-flush term)
        #t
      ) ;let*
    ) ;define

    (define (alt-screen-render! screen lines . opt-cursor)
      (%check-alt-screen screen "alt-screen-render!")
      (let* ((term (%alt-screen-terminal screen))
             (buf (%alt-screen-buffer screen))
             (cur (and (pair? opt-cursor) (car opt-cursor)))
             (cursor-same? (%cursor-same? term cur))
             (old-lines (screen-buffer-lines buf))
             ;; 快速路径检测：至多一行变化（Working 动画轻量 tick 等场景，
             ;; 其余行与上帧共享同一字符串对象）。跳过全帧归一化与字符串 diff，
             ;; 仅归一化并输出该行，避免每拍 ~1.7ms 的全帧扫描。
             (changes (and (not (screen-buffer-dirty? buf)) (%single-line-changes old-lines lines))
             ) ;changes
            ) ;
        (cond
         ((and changes (not (null? changes)))
          (%render-changed-line! screen buf term cur changes old-lines lines)
         ) ;
         ((and changes (null? changes) cur cursor-same?)
          ;; 全部行未变且光标位置未变：仅 commit（保持 buffer 同步），无输出
          (begin
            (screen-buffer-commit! buf lines)
            (terminal-flush term)
            #t
          ) ;begin
         ) ;
         (else (%render-full-frame! screen buf term cur lines))
        ) ;cond
      ) ;let*
    ) ;define

    (define (alt-screen-invalidate! screen)
      (%check-alt-screen screen "alt-screen-invalidate!")
      (screen-buffer-invalidate! (%alt-screen-buffer screen))
      #t
    ) ;define

    ;; 全量重绘：清屏后按当前帧内容重新渲染
    (define (alt-screen-refresh! screen)
      (%check-alt-screen screen "alt-screen-refresh!")
      (let* ((term (%alt-screen-terminal screen))
             (buf (%alt-screen-buffer screen))
             (lines (screen-buffer-lines buf))
            ) ;
        (screen-buffer-invalidate! buf)
        (terminal-clear term)
        (unless (null? lines)
          (alt-screen-render! screen lines)
        ) ;unless
        #t
      ) ;let*
    ) ;define

    (define (alt-screen-clear! screen)
      (%check-alt-screen screen "alt-screen-clear!")
      (let* ((term (%alt-screen-terminal screen)) (buf (%alt-screen-buffer screen)))
        (terminal-clear term)
        (screen-buffer-clear! buf)
        (terminal-flush term)
        #t
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

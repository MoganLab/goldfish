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

(define-library (liii tui editor handle)
  (export %editor-render %editor-handle-input)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii unicode)
    (liii tui component)
    (liii tui text-width)
    (liii tui box border)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor kill)
    (liii tui editor view)
    (liii tui editor state)
    (liii tui editor viewport)
    (liii tui editor paste)
    (liii tui editor undo)
    (liii tui editor edit)
  ) ;import
  (begin

    ;; 无角纯水平线边框样式，复用 box border 渲染机制
    (define *editor-h-border-style* (make-border-style "" "" "" "" "─" ""))

    (define (%make-h-border width color)
      (let ((raw (border-render-top *editor-h-border-style* width)))
        (if (string? color) (string-append color raw "\x1b;[0m") raw)
      ) ;let
    ) ;define

    (define %editor-scroll-dim "\x1b;[2m")
    (define %editor-scroll-reset "\x1b;[0m")

    (define (%editor-scroll-line arrow n)
      (string-append %editor-scroll-dim
        arrow
        " "
        (number->string n)
        " more"
        %editor-scroll-reset
      ) ;string-append
    ) ;define

    (define (%editor-render comp width)
      (let* ((data (component-data comp))
             (buf (%ed-buffer data))
             (raw-lines (buffer-lines buf))
             (row (buffer-row buf))
             (col (buffer-col buf))
             (focused? (%ed-focused? data))
             (ph (%ed-placeholder data))
             (cursor-style (%ed-cursor-style data))
             (top-b (%ed-top-border data))
             (bot-b (%ed-bottom-border data))
             (b-col (%ed-border-color data))
             (vp (%ed-viewport data width))
             (top (car vp))
             (visible (cadr vp))
             (above (caddr vp))
             (below (cadddr vp))
             (placeholder? (and (not focused?) (= (length raw-lines) 1) (string=? (car raw-lines) ""))
             ) ;placeholder?
             (visual-lines (append-map (lambda (line) (wrap-text line width)) raw-lines))
             (visible-lines (if placeholder? raw-lines (take (drop visual-lines top) visible))
             ) ;visible-lines
             (rendered
               (if placeholder?
                 (render-editor-lines visible-lines 0 0 #f ph width cursor-style)
                 (if (and focused? (not (eq? cursor-style 'hardware)))
                   (let* ((pos (editor-visual-cursor raw-lines row col width))
                          (cursor-row (- (car pos) top))
                          (cursor-col (cadr pos))
                         ) ;
                     (map (lambda (line r)
                            (if (= r cursor-row)
                              (render-line-with-cursor line cursor-col #t cursor-style)
                              line
                            ) ;if
                          ) ;lambda
                       visible-lines
                       (iota (length visible-lines))
                     ) ;map
                   ) ;let*
                   visible-lines
                 ) ;if
               ) ;if
             ) ;rendered
             (body
               (append (if (> above 0) (list (%editor-scroll-line "↑" above)) '())
                 rendered
                 (if (> below 0) (list (%editor-scroll-line "↓" below)) '())
               ) ;append
             ) ;body
            ) ;
        (if (and (> width 0) (or top-b bot-b))
          (append (if top-b (list (%make-h-border width b-col)) '())
            body
            (if bot-b (list (%make-h-border width b-col)) '())
          ) ;append
          body
        ) ;if
      ) ;let*
    ) ;define

    ;; -----------------------------------------------------------------
    ;; 历史浏览方向键
    ;; -----------------------------------------------------------------

    ;; 多行编辑器且光标不在行首时，↑ 先跳到行首（再按一次即回溯历史）
    (define (%editor-up-move-home? buf hist multi?)
      (and multi?
        (not (or (%editor-empty? buf) (history-index hist) (= (buffer-col buf) 0)))
      ) ;and
    ) ;define

    ;; ↑ 键：光标在首行时回溯输入历史，否则上移光标
    ;; 单行编辑器保持经典 readline 行为：↑ 直接回溯历史。
    ;; 首次进入历史浏览时记录快照，Ctrl+Z 可一步回到草稿；
    ;; entering? 必须先于 history-prev 求值（后者会改动 index）；
    ;; 草稿保存展开后的文本：down 返回草稿时不会还原出已失效的标记
    (define (%editor-up! data buf comp on-ch hist multi?)
      (if (%editor-first-line? buf)
        (if (%editor-up-move-home? buf hist multi?)
          (buffer-move-home! buf)
          (let ((entering? (not (history-index hist))))
            (%load-history! data
              buf
              comp
              on-ch
              (history-prev hist (paste-expand (%ed-pastes data) (buffer-text buf)))
              entering?
            ) ;%load-history!
          ) ;let
        ) ;if
        (buffer-move-up! buf)
      ) ;if
    ) ;define

    ;; ↓ 键：浏览历史中且位于末行 → 回退历史；
    ;;       位于末行 → 跳到行尾；否则下移光标
    ;;       （对齐上游 cursorDown 分支）
    (define (%editor-down! data buf comp on-ch hist)
      (if (and (history-index hist) (%editor-last-line? buf))
        (%load-history! data
          buf
          comp
          on-ch
          (history-next hist (paste-expand (%ed-pastes data) (buffer-text buf)))
          #f
        ) ;%load-history!
        (if (%editor-last-line? buf) (buffer-move-end! buf) (buffer-move-down! buf))
      ) ;if
    ) ;define

    ;; Enter 提交：提交前展开隐藏的粘贴内容；提交后清空撤销栈
    ;; （新一轮输入从零开始）
    (define (%editor-submit! data buf comp on-sub hist)
      (let ((txt (paste-expand (%ed-pastes data) (buffer-text buf))))
        (history-push! hist txt)
        (when (procedure? on-sub)
          (on-sub comp txt)
        ) ;when
        (%editor-undo-clear! data)
        #t
      ) ;let
    ) ;define

    (define (%editor-handle-input comp key)
      (let* ((data (component-data comp))
             (buf (%ed-buffer data))
             (hist (%ed-history data))
             (multi? (%ed-multiline? data))
             (submit-on-enter? (%ed-submit-on-enter? data))
             (on-ch (%ed-on-change data))
             (on-sub (%ed-on-submit data))
            ) ;
        (let ((res
                (cond ((eq? key 'ctrl-z)
                       ;; 撤销：恢复最近快照（栈空时无操作），键始终被消费
                       (%editor-undo! comp data)
                       #t
                      ) ;
                      ((or (eq? key 'left) (eq? key 'ctrl-b)) (buffer-move-left! buf))
                      ((or (eq? key 'right) (eq? key 'ctrl-f)) (buffer-move-right! buf))
                      ;; 行首/行尾：与上游 tui-editor 一致，只绑定 home/ctrl+a 与 end/ctrl+e；
                      ;; ctrl+home/ctrl+end 留给全屏视口（跳到记录顶部/末尾）
                      ((or (eq? key 'home) (eq? key 'ctrl-a)) (buffer-move-home! buf))
                      ((or (eq? key 'end) (eq? key 'ctrl-e)) (buffer-move-end! buf))
                      ((or (eq? key 'ctrl-left) (eq? key 'alt-b) (eq? key 'alt-left))
                       (buffer-move-word-left! buf)
                      ) ;
                      ((or (eq? key 'ctrl-right) (eq? key 'alt-f) (eq? key 'alt-right))
                       (buffer-move-word-right! buf)
                      ) ;
                      ((or (eq? key 'up) (eq? key 'ctrl-p))
                       (%editor-up! data buf comp on-ch hist multi?)
                      ) ;
                      ((or (eq? key 'down) (eq? key 'ctrl-n))
                       (%editor-down! data buf comp on-ch hist)
                      ) ;
                      ((or (eq? key 'backspace) (eq? key 'ctrl-h))
                       (or (%try-backspace-marker! comp data buf on-ch)
                         (%apply-edit! data comp buf on-ch (lambda () (buffer-backspace! buf)))
                       ) ;or
                      ) ;
                      ((or (eq? key 'delete) (eq? key 'ctrl-d))
                       (or (%try-delete-marker! comp data buf on-ch)
                         (%apply-edit! data comp buf on-ch (lambda () (buffer-delete! buf)))
                       ) ;or
                      ) ;
                      ((eq? key 'ctrl-k)
                       (%apply-edit! data comp buf on-ch (lambda () (buffer-kill-line-right! buf)))
                      ) ;
                      ((eq? key 'ctrl-u)
                       (%apply-edit! data comp buf on-ch (lambda () (buffer-kill-line-left! buf)))
                      ) ;
                      ((or (eq? key 'ctrl-w) (eq? key 'alt-backspace))
                       (%apply-edit! data comp buf on-ch (lambda () (buffer-kill-word-left! buf)))
                      ) ;
                      ((eq? key 'ctrl-t)
                       (%apply-edit! data comp buf on-ch (lambda () (buffer-transpose-chars! buf)))
                      ) ;
                      ((eq? key 'ctrl-j)
                       ;; 多行模式下的显式换行键（Enter 已被提交占用）
                       (if multi?
                         (%apply-edit! data comp buf on-ch (lambda () (buffer-newline! buf)))
                         #f
                       ) ;if
                      ) ;
                      ((eq? key 'enter)
                       ;; Enter 优先提交；submit-on-enter? 关闭时多行模式插入换行
                       (if submit-on-enter?
                         (%editor-submit! data buf comp on-sub hist)
                         (if multi?
                           (%apply-edit! data comp buf on-ch (lambda () (buffer-newline! buf)))
                           #f
                         ) ;if
                       ) ;if
                      ) ;
                      ((and (string? key) (%bracketed-paste? key))
                       (%handle-paste! comp data buf on-ch (%bracketed-paste-content key))
                      ) ;
                      ((string? key)
                       (%editor-undo-type! data key)
                       (buffer-insert-string! buf key)
                       (when (procedure? on-ch)
                         (on-ch comp (buffer-text buf))
                       ) ;when
                       #t
                      ) ;
                      ((char? key)
                       (%editor-undo-type! data (string key))
                       (buffer-insert-char! buf key)
                       (when (procedure? on-ch)
                         (on-ch comp (buffer-text buf))
                       ) ;when
                       #t
                      ) ;
                      (else #f)
                ) ;cond
              ) ;res
             ) ;
          (%ed-sync-viewport! data 0)
          res
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

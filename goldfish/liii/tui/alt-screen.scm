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
    alt-screen-render! alt-screen-clear! alt-screen-invalidate!
    alt-screen-refresh!
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
               (terminal-write term (diff-op-text op))
               ;; 清掉行尾残留：全屏帧宽固定，行内容变短时必须擦除余下部分
               (terminal-clear-line-to-end term)
              ) ;
              ((eq? type 'clear) (terminal-set-cursor! term 0 idx) (terminal-clear-line term))
              (else #t)
        ) ;cond
      ) ;let
    ) ;define

    ;; 渲染一整帧：lines 为按行排列的帧内容，可选光标位置 '(x . y)
    (define (alt-screen-render! screen lines . opt-cursor)
      (%check-alt-screen screen "alt-screen-render!")
      (let* ((term (%alt-screen-terminal screen))
             (buf (%alt-screen-buffer screen))
             (frame (%alt-frame term lines))
             (old-lines (screen-buffer-lines buf))
             (dirty? (screen-buffer-dirty? buf))
             (diff-ops (if dirty? (screen-dirty-diff old-lines frame) (screen-diff old-lines frame))
             ) ;diff-ops
             (w (car (terminal-size term)))
            ) ;
        (when (screen-diff-has-changes? diff-ops)
          (terminal-begin-sync! term)
          (for-each (lambda (op) (%render-op! term op w)) diff-ops)
          (terminal-end-sync! term)
        ) ;when
        (screen-buffer-commit! buf frame)
        (let ((cur (and (pair? opt-cursor) (car opt-cursor))))
          (when cur
            (terminal-set-cursor! term (car cur) (cdr cur))
            (unless (terminal-cursor-visible? term)
              (terminal-show-cursor! term)
            ) ;unless
          ) ;when
        ) ;let
        (terminal-flush term)
        #t
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

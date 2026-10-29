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

(define-library (liii tui editor edit)
  (export %apply-edit! %load-history! %editor-first-line? %editor-last-line?
    %editor-empty? %handle-paste! %try-backspace-marker! %try-delete-marker!
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii unicode)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor paste)
    (liii tui editor state)
    (liii tui editor undo)
  ) ;import
  (begin

    ;; 编辑前先取快照，编辑生效则入撤销栈（编辑未生效不产生空快照）
    (define (%apply-edit! data comp buf on-ch thunk)
      (let* ((snap (%editor-snapshot data)) (res (thunk)))
        (when res
          (%ed-undo-stack-set! data (cons snap (%ed-undo-stack data)))
          (%ed-last-action-set! data #f)
        ) ;when
        (when (and res (procedure? on-ch))
          (on-ch comp (buffer-text buf))
        ) ;when
        res
      ) ;let*
    ) ;define

    ;; push-undo? 为真时记录浏览前快照（仅首次进入历史浏览时传入，
    ;; 对齐上游 navigateHistory 只在进入浏览时入栈），使 Ctrl+Z 直接回到草稿；
    ;; 快照先于注册表清空记录，撤销时文本与粘贴注册表一并还原
    (define (%load-history! data buf comp on-ch text push-undo?)
      (when text
        (when push-undo?
          (%ed-undo-stack-set! data (cons (%editor-snapshot data) (%ed-undo-stack data)))
          (%ed-last-action-set! data #f)
        ) ;when
        ;; 历史整体替换缓冲区后，旧标记全部失效；清掉注册表，
        ;; 避免之后手打的标记文本被误展开
        (paste-registry-clear! (%ed-pastes data))
        (buffer-set-text! buf text)
        ;; 光标置于行尾：与既有行为一致（回溯历史后继续编辑末尾）；
        ;; 上游 navigateHistory 向上时置于行首，此处保持行尾以便连续回溯
        ;; （浏览历史态下 ↑ 直接继续回溯，不依赖光标列位置）
        (buffer-move-end! buf)
        (when (procedure? on-ch)
          (on-ch comp text)
        ) ;when
      ) ;when
      (if text #t #f)
    ) ;define

    ;; 光标是否位于首个逻辑行（对齐上游 isOnFirstVisualLine 的逻辑行近似）
    (define (%editor-first-line? buf)
      (= (buffer-row buf) 0)
    ) ;define

    ;; 光标是否位于末个逻辑行（对齐上游 isOnLastVisualLine 的逻辑行近似）
    (define (%editor-last-line? buf)
      (= (buffer-row buf) (- (length (buffer-lines buf)) 1))
    ) ;define

    ;; 缓冲区是否为空（单行且空串，对齐上游 isEditorEmpty）
    (define (%editor-empty? buf)
      (and (= (length (buffer-lines buf)) 1) (string=? (car (buffer-lines buf)) ""))
    ) ;define

    ;; -----------------------------------------------------------------
    ;; 粘贴：大段内容折叠为 [paste #N ...]，提交时由 paste-expand 展开
    ;; -----------------------------------------------------------------

    (define (%handle-paste! comp data buf on-ch text)
      (let ((clean (%paste-normalize text)))
        (if (string=? clean "")
          #t
          (begin
            ;; 粘贴为原子操作：记录粘贴前快照，撤销可整体还原（含注册表）
            (let ((snap (%editor-snapshot data)))
              (if (%paste-large? clean)
                (buffer-insert-text! buf (paste-registry-add! (%ed-pastes data) clean))
                (buffer-insert-text! buf clean)
              ) ;if
              (%ed-undo-stack-set! data (cons snap (%ed-undo-stack data)))
              (%ed-last-action-set! data #f)
            ) ;let
            (when (procedure? on-ch)
              (on-ch comp (buffer-text buf))
            ) ;when
            #t
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (%current-line buf)
      (list-ref (buffer-lines buf) (buffer-row buf))
    ) ;define

    (define (%remove-span! buf start end)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
             (before (if (<= start 0) "" (utf8-substring line 0 start)))
             (after (if (>= end len) "" (utf8-substring line end len)))
             (new-line (string-append before after))
            ) ;
        (buffer-lines-set! buf (%replace-line lines r new-line))
        #t
      ) ;let*
    ) ;define

    ;; 光标前是有效标记时整体退格删除
    (define (%try-backspace-marker! comp data buf on-ch)
      (let* ((reg (%ed-pastes data))
             (line (%current-line buf))
             (col (buffer-col buf))
             (span (paste-marker-at-end reg line col))
            ) ;
        (when span
          (let ((snap (%editor-snapshot data)))
            (%remove-span! buf (cdr span) col)
            (paste-registry-remove! reg (car span))
            (buffer-col-set! buf (min (cdr span) (utf8-string-length (%current-line buf))))
            (%ed-undo-stack-set! data (cons snap (%ed-undo-stack data)))
            (%ed-last-action-set! data #f)
            (when (procedure? on-ch)
              (on-ch comp (buffer-text buf))
            ) ;when
          ) ;let
        ) ;when
        (if span #t #f)
      ) ;let*
    ) ;define

    ;; Delete 键删除光标处的有效标记
    (define (%try-delete-marker! comp data buf on-ch)
      (let* ((reg (%ed-pastes data))
             (line (%current-line buf))
             (col (buffer-col buf))
             (span (paste-marker-at-start reg line col))
            ) ;
        (when span
          (let ((snap (%editor-snapshot data)))
            (%remove-span! buf col (cdr span))
            (paste-registry-remove! reg (car span))
            (%ed-undo-stack-set! data (cons snap (%ed-undo-stack data)))
            (%ed-last-action-set! data #f)
            (when (procedure? on-ch)
              (on-ch comp (buffer-text buf))
            ) ;when
          ) ;let
        ) ;when
        (if span #t #f)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

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
  (export editor-handle-input)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii unicode)
    (liii tui component)
    (liii tui editor history)
    (liii tui editor buffer)
    (liii tui editor cursor)
    (liii tui editor kill)
    (liii tui editor state)
    (liii tui editor viewport)
    (liii tui editor paste)
    (liii tui editor undo)
  ) ;import
  (begin

    ;; -----------------------------------------------------------------
    ;; 编辑原语
    ;; -----------------------------------------------------------------

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

    ;; -----------------------------------------------------------------
    ;; 按键分发
    ;; -----------------------------------------------------------------

    (define (editor-handle-input comp key)
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

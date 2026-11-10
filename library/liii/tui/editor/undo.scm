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

;; 编辑器撤销（对齐上游 pi-mono tui.editor.undo）：
;; 快照 = 编辑前状态 (lines row col pastes-snapshot)；
;; lines 与 pastes 注册表内容均为不可变 list，各编辑操作一律重建新 list，
;; 快照持引用即等价深拷贝。
;; 打字输入按 fish 式合并：连续非空白字符共享同一快照，空白自成单元。
(define-library (liii tui editor undo)
  (export %editor-snapshot %editor-undo-type! %editor-undo! %editor-undo-clear!)
  (import (scheme base)
    (scheme char)
    (liii tui editor state)
    (liii tui editor buffer)
    (liii tui editor paste)
  ) ;import
  (begin

    (define (%editor-snapshot data)
      (let ((buf (%ed-buffer data)))
        (list (buffer-lines buf)
          (buffer-row buf)
          (buffer-col buf)
          (paste-registry-snapshot (%ed-pastes data))
        ) ;list
      ) ;let
    ) ;define

    (define (%restore! data snap)
      (let ((buf (%ed-buffer data)))
        (buffer-lines-set! buf (list-ref snap 0))
        (buffer-row-set! buf (list-ref snap 1))
        (buffer-col-set! buf (list-ref snap 2))
        (paste-registry-restore! (%ed-pastes data) (list-ref snap 3))
      ) ;let
    ) ;define

    ;; 是否全为空白（含空串，视为需单独成单元）
    (define (%all-whitespace? text)
      (let loop
        ((i 0) (len (string-length text)))
        (cond ((= i len) #t)
              ((char-whitespace? (string-ref text i)) (loop (+ i 1) len))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 打字输入的快照记录（fish 式合并）：
    ;; 连续非空白字符合并为一个撤销单元；空白输入前先记快照，
    ;; 使撤销时空格与随后的词一起被移除
    (define (%editor-undo-type! data text)
      (unless (and (not (%all-whitespace? text)) (eq? (%ed-last-action data) 'type-word))
        (%ed-undo-stack-set! data (cons (%editor-snapshot data) (%ed-undo-stack data)))
      ) ;unless
      (%ed-last-action-set! data 'type-word)
    ) ;define

    ;; Ctrl+Z：弹出最近快照并恢复，栈空时不改变内容；恢复后触发 on-change
    (define (%editor-undo! comp data)
      (let ((stack (%ed-undo-stack data)))
        (if (null? stack)
          #f
          (begin
            (%restore! data (car stack))
            (%ed-undo-stack-set! data (cdr stack))
            (%ed-last-action-set! data #f)
            (let ((on-ch (%ed-on-change data)))
              (when (procedure? on-ch)
                (on-ch comp (buffer-text (%ed-buffer data)))
              ) ;when
            ) ;let
            #t
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    ;; 清空撤销栈（提交后新一轮输入从零开始，对齐上游 submitValue）
    (define (%editor-undo-clear! data)
      (%ed-undo-stack-set! data '())
      (%ed-last-action-set! data #f)
      #t
    ) ;define

  ) ;begin
) ;define-library

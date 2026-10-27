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

(define-library (liii tui editor buffer)
  (export make-buffer buffer? buffer-lines buffer-row buffer-col
    buffer-multiline? buffer-lines-set! buffer-row-set! buffer-col-set!
    buffer-text buffer-set-text! buffer-insert-char! buffer-insert-string!
    buffer-backspace! buffer-delete! buffer-newline! %replace-line
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
  ) ;import
  (begin

    (define-record-type <buffer>
      (%make-buffer-record lines row col multiline?)
      buffer?
      (lines %buffer-lines %buffer-lines-set!)
      (row %buffer-row %buffer-row-set!)
      (col %buffer-col %buffer-col-set!)
      (multiline? %buffer-multiline? %buffer-multiline?-set!)
    ) ;define-record-type

    (define (make-buffer . args)
      (let ((initial-text (if (pair? args) (car args) ""))
            (multi?
              (if (and (pair? args) (pair? (cdr args))) (cadr args) #t)
            ) ;multi?
           ) ;
        (let* ((raw-lines (string-split initial-text "\n"))
               (lines (if (null? raw-lines) (list "") raw-lines))
              ) ;
          (%make-buffer-record lines 0 0 multi?)
        ) ;let*
      ) ;let
    ) ;define

    (define (buffer-lines buf)
      (%buffer-lines buf)
    ) ;define
    (define (buffer-row buf)
      (%buffer-row buf)
    ) ;define
    (define (buffer-col buf)
      (%buffer-col buf)
    ) ;define
    (define (buffer-multiline? buf)
      (%buffer-multiline? buf)
    ) ;define
    (define (buffer-lines-set! buf l)
      (%buffer-lines-set! buf l)
    ) ;define
    (define (buffer-row-set! buf r)
      (%buffer-row-set! buf r)
    ) ;define
    (define (buffer-col-set! buf c)
      (%buffer-col-set! buf c)
    ) ;define

    (define (buffer-text buf)
      (string-join (%buffer-lines buf) "\n")
    ) ;define

    (define (buffer-set-text! buf text)
      (let* ((raw (string-split text "\n")) (lines (if (null? raw) (list "") raw)))
        (%buffer-lines-set! buf lines)
        (%buffer-row-set! buf 0)
        (%buffer-col-set! buf 0)
      ) ;let*
    ) ;define

    ;; 辅助：更新某行的内容
    (define (%replace-line lines idx new-line)
      (map (lambda (line i) (if (= i idx) new-line line)) lines (iota (length lines)))
    ) ;define

    ;; 插入字符串到当前光标 (row, col)
    (define (buffer-insert-string! buf str)
      (if (string=? str "")
        #t
        (let* ((lines (%buffer-lines buf))
               (r (%buffer-row buf))
               (c (%buffer-col buf))
               (line (list-ref lines r))
               (len (utf8-string-length line))
               (safe-col (max 0 (min c len)))
               (before (utf8-substring line 0 safe-col))
               (after (if (>= safe-col len) "" (utf8-substring line safe-col len)))
               (new-line (string-append before str after))
              ) ;
          (%buffer-lines-set! buf (%replace-line lines r new-line))
          (%buffer-col-set! buf (+ safe-col (utf8-string-length str)))
          #t
        ) ;let*
      ) ;if
    ) ;define

    (define (buffer-insert-char! buf ch)
      (buffer-insert-string! buf (string ch))
    ) ;define

    ;; 退格删除（向左删除字符或向上合并行）
    (define (buffer-backspace! buf)
      (let* ((lines (%buffer-lines buf))
             (r (%buffer-row buf))
             (c (%buffer-col buf))
             (line (list-ref lines r))
            ) ;
        (cond
         ((> c 0)
          ;; 行内向左删除一个字符
          (let* ((len (utf8-string-length line))
                 (safe-col (min c len))
                 (before (utf8-substring line 0 (- safe-col 1)))
                 (after (if (>= safe-col len) "" (utf8-substring line safe-col len)))
                 (new-line (string-append before after))
                ) ;
            (%buffer-lines-set! buf (%replace-line lines r new-line))
            (%buffer-col-set! buf (- safe-col 1))
            #t
          ) ;let*
         ) ;
         ((and (> r 0) (%buffer-multiline? buf))
          ;; 光标在第 r 行首（c=0），将当前行拼接到上一行末尾
          (let* ((prev-line (list-ref lines (- r 1)))
                 (prev-len (utf8-string-length prev-line))
                 (merged (string-append prev-line line))
                ) ;
            (%buffer-lines-set! buf
              (append (take lines (- r 1)) (list merged) (drop lines (+ r 1)))
            ) ;%buffer-lines-set!
            (%buffer-row-set! buf (- r 1))
            (%buffer-col-set! buf prev-len)
            #t
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 向后删除（Delete 键：向右删除字符或合并下一行）
    (define (buffer-delete! buf)
      (let* ((lines (%buffer-lines buf))
             (r (%buffer-row buf))
             (c (%buffer-col buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
            ) ;
        (cond
         ((< c len)
          ;; 行内向右删除一个字符
          (let* ((before (utf8-substring line 0 c))
                 (after (if (>= (+ c 1) len) "" (utf8-substring line (+ c 1) len)))
                 (new-line (string-append before after))
                ) ;
            (%buffer-lines-set! buf (%replace-line lines r new-line))
            #t
          ) ;let*
         ) ;
         ((and (< r (- (length lines) 1)) (%buffer-multiline? buf))
          ;; 光标在行尾，将下一行拼接到当前行
          (let* ((next-line (list-ref lines (+ r 1))) (merged (string-append line next-line)))
            (%buffer-lines-set! buf
              (append (take lines r) (list merged) (drop lines (+ r 2)))
            ) ;%buffer-lines-set!
            #t
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 多行模式下回车换行：将当前行从光标处分裂为两行
    (define (buffer-newline! buf)
      (if (not (%buffer-multiline? buf))
        #f
        (let* ((lines (%buffer-lines buf))
               (r (%buffer-row buf))
               (c (%buffer-col buf))
               (line (list-ref lines r))
               (len (utf8-string-length line))
               (safe-col (max 0 (min c len)))
               (line1 (utf8-substring line 0 safe-col))
               (line2 (if (>= safe-col len) "" (utf8-substring line safe-col len)))
              ) ;
          (%buffer-lines-set! buf
            (append (take lines r) (list line1 line2) (drop lines (+ r 1)))
          ) ;%buffer-lines-set!
          (%buffer-row-set! buf (+ r 1))
          (%buffer-col-set! buf 0)
          #t
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

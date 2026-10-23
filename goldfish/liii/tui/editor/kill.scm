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

(define-library (liii tui editor kill)
  (export buffer-kill-line-right! buffer-kill-line-left! buffer-kill-word-left!
    buffer-transpose-chars!
  ) ;export
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui editor buffer)
  ) ;import
  (begin

    (define (%replace-line lines idx new-line)
      (let loop
        ((rest lines) (i 0))
        (cond ((null? rest) '())
              ((= i idx) (cons new-line (cdr rest)))
              (else (cons (car rest) (loop (cdr rest) (+ i 1))))
        ) ;cond
      ) ;let
    ) ;define

    ;; 删除光标到行尾 (Ctrl-K)
    (define (buffer-kill-line-right! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
            ) ;
        (if (>= c len)
          #f
          (let ((new-line (utf8-substring line 0 c)))
            (buffer-lines-set! buf (%replace-line lines r new-line))
            #t
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 删除光标到行首 (Ctrl-U)
    (define (buffer-kill-line-left! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
            ) ;
        (if (<= c 0)
          #f
          (let ((new-line (if (>= c len) "" (utf8-substring line c len))))
            (buffer-lines-set! buf (%replace-line lines r new-line))
            (buffer-col-set! buf 0)
            #t
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 向左删除一个词 (Ctrl-W / Alt-Backspace)
    (define (buffer-kill-word-left! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (chars (string->list line))
             (len (length chars))
            ) ;
        (if (<= c 0)
          #f
          (let* ((safe-c (min c len))
                 (pos1
                   (let loop
                     ((i safe-c))
                     (if
                       (and (> i 0) (char=? (list-ref chars (- i 1)) #\space))
                       (loop (- i 1))
                       i
                     ) ;if
                   ) ;let
                 ) ;pos1
                 (pos2
                   (let loop
                     ((i pos1))
                     (if
                       (and (> i 0) (not (char=? (list-ref chars (- i 1)) #\space)))
                       (loop (- i 1))
                       i
                     ) ;if
                   ) ;let
                 ) ;pos2
                 (before (list->string (take chars pos2)))
                 (after (list->string (drop chars safe-c)))
                 (new-line (string-append before after))
                ) ;
            (buffer-lines-set! buf (%replace-line lines r new-line))
            (buffer-col-set! buf pos2)
            #t
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; 交换光标前两个字符 (Ctrl-T)
    (define (buffer-transpose-chars! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (chars (string->list line))
             (len (length chars))
            ) ;
        (cond
         ((and (>= c 2) (<= c len))
          (let* ((c1 (list-ref chars (- c 2)))
                 (c2 (list-ref chars (- c 1)))
                 (before (take chars (- c 2)))
                 (after (drop chars c))
                 (new-chars (append before (list c2 c1) after))
                ) ;
            (buffer-lines-set! buf (%replace-line lines r (list->string new-chars)))
            #t
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

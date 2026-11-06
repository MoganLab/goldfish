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
    (liii tui editor cursor)
  ) ;import
  (begin

    ;; 删除光标到行尾 (Ctrl-K)；光标已在行尾时删除换行、
    ;; 把下一行拉到当前行（对空行即删除该空行）
    (define (buffer-kill-line-right! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
            ) ;
        (cond
         ((< c len)
          (buffer-lines-set! buf (%replace-line lines r (utf8-substring line 0 c)))
          #t
         ) ;
         ((< r (- (length lines) 1))
          ;; 行尾且非末行：与下一行合并（同 Delete 键行尾语义）
          (let ((merged (string-append line (list-ref lines (+ r 1)))))
            (buffer-lines-set! buf
              (append (take lines r) (list merged) (drop lines (+ r 2)))
            ) ;buffer-lines-set!
            #t
          ) ;let
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 删除光标到行首 (Ctrl-U)；光标已在行首时删除换行、
    ;; 回退到上一行末尾（对空行即删除该空行）
    (define (buffer-kill-line-left! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (len (utf8-string-length line))
            ) ;
        (cond
         ((> c 0)
          (buffer-lines-set! buf
            (%replace-line lines r (if (>= c len) "" (utf8-substring line c len)))
          ) ;buffer-lines-set!
          (buffer-col-set! buf 0)
          #t
         ) ;
         ((> r 0)
          ;; 行首且非首行：与上一行合并（同 Backspace 键行首语义）
          (let* ((prev-line (list-ref lines (- r 1))) (merged (string-append prev-line line)))
            (buffer-lines-set! buf
              (append (take lines (- r 1)) (list merged) (drop lines (+ r 1)))
            ) ;buffer-lines-set!
            (buffer-row-set! buf (- r 1))
            (buffer-col-set! buf (utf8-string-length prev-line))
            #t
          ) ;let*
         ) ;
         (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 向左删除一个词 (Ctrl-W / Alt-Backspace)
    (define (buffer-kill-word-left! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (line (list-ref lines r))
             (pos (%word-left-boundary line c))
            ) ;
        (if (= pos c)
          #f
          (let* ((len (string-length line))
                 (new-line (string-append (substring line 0 pos) (if (>= c len) "" (substring line c len)))
                 ) ;new-line
                ) ;
            (buffer-lines-set! buf (%replace-line lines r new-line))
            (buffer-col-set! buf pos)
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
             (len (string-length line))
            ) ;
        (if (and (>= c 2) (<= c len))
          (let* ((before (substring line 0 (- c 2)))
                 (c1 (string-ref line (- c 2)))
                 (c2 (string-ref line (- c 1)))
                 (after (substring line c len))
                 (new-line (string-append before (string c2 c1) after))
                ) ;
            (buffer-lines-set! buf (%replace-line lines r new-line))
            #t
          ) ;let*
          #f
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

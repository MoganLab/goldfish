;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2024-2026 The Goldfish Scheme Authors
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

(define-library (liii tui editor cursor)
  (export buffer-move-left! buffer-move-right! buffer-move-up! buffer-move-down!
    buffer-move-home! buffer-move-end!
  ) ;export
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii string)
    (liii tui editor buffer)
  ) ;import
  (begin

    ;; 光标移动：左
    (define (buffer-move-left! buf)
      (let ((r (buffer-row buf)) (c (buffer-col buf)))
        (cond ((> c 0) (buffer-col-set! buf (- c 1)) #t)
              ((and (> r 0) (buffer-multiline? buf))
               (let* ((prev-r (- r 1))
                      (prev-len (utf8-string-length (list-ref (buffer-lines buf) prev-r)))
                     ) ;
                 (buffer-row-set! buf prev-r)
                 (buffer-col-set! buf prev-len)
                 #t
               ) ;let*
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    ;; 光标移动：右
    (define (buffer-move-right! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (c (buffer-col buf))
             (len (utf8-string-length (list-ref lines r)))
            ) ;
        (cond ((< c len) (buffer-col-set! buf (+ c 1)) #t)
              ((and (< r (- (length lines) 1)) (buffer-multiline? buf))
               (buffer-row-set! buf (+ r 1))
               (buffer-col-set! buf 0)
               #t
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 光标移动：Home 行首
    (define (buffer-move-home! buf)
      (buffer-col-set! buf 0)
      #t
    ) ;define

    ;; 光标移动：End 行尾
    (define (buffer-move-end! buf)
      (let* ((lines (buffer-lines buf))
             (r (buffer-row buf))
             (len (utf8-string-length (list-ref lines r)))
            ) ;
        (buffer-col-set! buf len)
        #t
      ) ;let*
    ) ;define

    ;; 光标移动：Up 上移一行，列位置贴近目标行长度（吸附）
    (define (buffer-move-up! buf)
      (let ((r (buffer-row buf)))
        (if (<= r 0)
          #f
          (let* ((new-r (- r 1))
                 (target-len (utf8-string-length (list-ref (buffer-lines buf) new-r)))
                 (new-c (min (buffer-col buf) target-len))
                ) ;
            (buffer-row-set! buf new-r)
            (buffer-col-set! buf new-c)
            #t
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 光标移动：Down 下移一行，列位置贴近目标行长度（吸附）
    (define (buffer-move-down! buf)
      (let* ((lines (buffer-lines buf)) (r (buffer-row buf)) (total-rows (length lines)))
        (if (>= r (- total-rows 1))
          #f
          (let* ((new-r (+ r 1))
                 (target-len (utf8-string-length (list-ref lines new-r)))
                 (new-c (min (buffer-col buf) target-len))
                ) ;
            (buffer-row-set! buf new-r)
            (buffer-col-set! buf new-c)
            #t
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

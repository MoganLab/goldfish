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

(define-library (liii tui text-width truncate)
  (export truncate-to-width)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii unicode)
    (liii tui text-width core)
  ) ;import
  (begin

    (define (%sub-ellipsis ellipsis width)
      (let loop
        ((chars (string->list ellipsis)) (acc '()) (cw 0))
        (if (or (null? chars) (>= cw width))
          (list->string (reverse acc))
          (loop (cdr chars) (cons (car chars) acc) (+ cw 1))
        ) ;if
      ) ;let
    ) ;define

    (define (%finalize-truncated acc ellipsis had-ansi?)
      (let ((base (apply string-append (reverse (cons ellipsis acc)))))
        (if had-ansi? (string-append base "\x1b;[0m") base)
      ) ;let
    ) ;define

    (define (%truncate-loop bv len target-w ellipsis)
      (let loop
        ((i 0) (cur-w 0) (acc '()) (had-ansi? #f))
        (cond ((>= i len) (%finalize-truncated acc ellipsis had-ansi?))
              ((%match-ansi-csi bv i len)
               =>
               (lambda (ansi-end)
                 (let ((chunk (utf8->string (bytevector-copy bv i ansi-end))))
                   (loop ansi-end cur-w (cons chunk acc) #t)
                 ) ;let
               ) ;lambda
              ) ;
              (else
                (let* ((next (bytevector-advance-utf8 bv i len))
                       (step (if (= next i) (+ i 1) next))
                       (cp (utf8->codepoint-at bv i))
                       (cw (%codepoint-width cp))
                      ) ;
                  (if (> (+ cur-w cw) target-w)
                    (%finalize-truncated acc ellipsis had-ansi?)
                    (let ((chunk (utf8->string (bytevector-copy bv i step))))
                      (loop step (+ cur-w cw) (cons chunk acc) had-ansi?)
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (truncate-to-width str width . args)
      (unless (string? str)
        (type-error "truncate-to-width: str must be a string")
      ) ;unless
      (unless (real? width)
        (type-error "truncate-to-width: width must be a number")
      ) ;unless
      ;; 非法 UTF-8 字节替换为 U+FFFD：宽度计算与截断输出都基于合法文本，
      ;; 与渲染管线的清理行为一致（合法输入返回原串对象，无额外分配）
      (let* ((str (utf8-replace-invalid str)) (ellipsis (if (null? args) "..." (car args))))
        (unless (string? ellipsis)
          (type-error "truncate-to-width: ellipsis must be a string")
        ) ;unless
        (let ((w (inexact->exact (floor width))))
          (cond ((<= w 0) "")
                ((string=? str "") "")
                ((<= (display-width str) w) str)
                (else
                  (let ((target-w (- w (display-width ellipsis))))
                    (if (< target-w 0)
                      (%sub-ellipsis ellipsis w)
                      (let ((bv (string->byte-vector str)))
                        (%truncate-loop bv (bytevector-length bv) target-w ellipsis)
                      ) ;let
                    ) ;if
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

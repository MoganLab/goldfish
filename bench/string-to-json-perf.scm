;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; string->json 性能对比基准：C++ 实现 (g_string->json) vs 历史 Scheme 实现
;; 运行方式: bin/gf bench/string-to-json-perf.scm

(import (liii base) (liii json) (liii timeit) (liii unicode))

;; 历史 Scheme 实现（原 (guenchi json) 中的 string->json 及其私有辅助函数），原样保留用于对比

(define (string-length-sum strings)
  (let loop
    ((o 0) (rest strings))
    (cond ((eq? '() rest) o)
          (else (loop (+ o (string-length (car rest))) (cdr rest)))
    ) ;cond
  ) ;let
) ;define

(define (fast-string-list-append strings)
  (let* ((output-length (string-length-sum strings))
         (output (make-string output-length #\_))
         (fill 0)
        ) ;
    (let outer
      ((rest strings))
      (cond ((eq? '() rest) output)
            (else (let* ((s (car rest)) (n (string-length s)))
                    (let inner
                      ((i 0))
                      (cond ((= i n) 'done)
                            (else (string-set! output fill (string-ref s i))
                              (set! fill (+ fill 1))
                              (inner (+ i 1))
                            ) ;else
                      ) ;cond
                    ) ;let
                  ) ;let*
              (outer (cdr rest))
            ) ;else
      ) ;cond
    ) ;let
  ) ;let*
) ;define

(define (handle-escape-char s end len)
  (let ((next-char (if (< (+ end 1) len) (string-ref s (+ end 1)) #f)))
    (case next-char
     ((#\") (values "\\\"" 2))
     ((#\\) (values "\\\\" 2))
     ((#\/) (values "/" 2))
     ((#\b) (values "\\b" 2))
     ((#\f) (values "\\f" 2))
     ((#\n) (values "\\n" 2))
     ((#\r) (values "\\r" 2))
     ((#\t) (values "\\t" 2))
     ((#\u)
      (let ((start-pos (+ end 2)) (end-pos (+ end 6)))
        (if (and (>= start-pos 0) (< end-pos len))
          (let ((hex-str (substring s start-pos end-pos)))
            (let ((code-point (string->number hex-str 16)))
              (when (not code-point)
                (error 'parse-error (string-append "Invalid HEX sequence " hex-str))
              ) ;when
              (let ((next-u-pos (+ end 6)))
                (if (and (< (+ next-u-pos 6) len)
                      (char=? (string-ref s next-u-pos) #\\)
                      (char=? (string-ref s (+ next-u-pos 1)) #\u)
                    ) ;and
                  (let ((next-hex-str (substring s (+ next-u-pos 2) (+ next-u-pos 6))))
                    (let ((next-code-point (string->number next-hex-str 16)))
                      (when (not next-code-point)
                        (error 'parse-error (string-append "Invalid HEX sequence " next-hex-str))
                      ) ;when
                      (if (and (>= code-point 55296)
                            (<= code-point 56319)
                            (>= next-code-point 56320)
                            (<= next-code-point 57343)
                          ) ;and
                        (let ((surrogate-code-point (+ (* (- code-point 55296) 1024) (- next-code-point 56320) 65536)
                              ) ;surrogate-code-point
                             ) ;
                          (values (utf8->string (codepoint->utf8 surrogate-code-point)) 12)
                        ) ;let
                        (values (utf8->string (codepoint->utf8 code-point)) 6)
                      ) ;if
                    ) ;let
                  ) ;let
                  (values (utf8->string (codepoint->utf8 code-point)) 6)
                ) ;if
              ) ;let
            ) ;let
          ) ;let
          (error 'parse-error
            (string-append "HEX sequence too short " (substring s start-pos))
          ) ;error
        ) ;if
      ) ;let
     ) ;
     (else (error 'parse-error (string-append "Invalid escape char: " (string next-char)))
     ) ;else
    ) ;case
  ) ;let
) ;define

(define string->json/scheme
  (lambda (s)
    (read (open-input-string (let loop
                               ((s s) (bgn 0) (end 0) (rst '()) (len (string-length s)) (quts? #f) (lst '(#t)))
                               (cond ((= end len) (fast-string-list-append (reverse rst)))
                                     ((and quts? (char=? (string-ref s end) #\\) (< (+ end 1) len))
                                      (let-values (((unescaped step) (handle-escape-char s end len)))
                                        (loop s
                                          (+ end step)
                                          (+ end step)
                                          (cons (string-append (substring s bgn end) unescaped) rst)
                                          len
                                          quts?
                                          lst
                                        ) ;loop
                                      ) ;let-values
                                     ) ;
                                     ((and quts? (not (char=? (string-ref s end) #\")))
                                      (loop s bgn (+ 1 end) rst len quts? lst)
                                     ) ;
                                     (else (case (string-ref s end)
                                                 ((#\{)
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) "((") rst)
                                                    len
                                                    quts?
                                                    (cons #t lst)
                                                  ) ;loop
                                                 ) ;
                                                 ((#\})
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) "))") rst)
                                                    len
                                                    quts?
                                                    (loose-cdr lst)
                                                  ) ;loop
                                                 ) ;
                                                 ((#\[)
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) "#(") rst)
                                                    len
                                                    quts?
                                                    (cons #f lst)
                                                  ) ;loop
                                                 ) ;
                                                 ((#\])
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) ")") rst)
                                                    len
                                                    quts?
                                                    (loose-cdr lst)
                                                  ) ;loop
                                                 ) ;
                                                 ((#\:)
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) " . ") rst)
                                                    len
                                                    quts?
                                                    lst
                                                  ) ;loop
                                                 ) ;
                                                 ((#\,)
                                                  (loop s
                                                    (+ 1 end)
                                                    (+ 1 end)
                                                    (cons (string-append (substring s bgn end) (if (loose-car lst) ")(" " ")) rst)
                                                    len
                                                    quts?
                                                    lst
                                                  ) ;loop
                                                 ) ;
                                                 ((#\") (loop s bgn (+ 1 end) rst len (not quts?) lst))
                                                 (else (loop s bgn (+ 1 end) rst len quts? lst))
                                           ) ;case
                                     ) ;else
                               ) ;cond
                             ) ;let
          ) ;open-input-string
    ) ;read
  ) ;lambda
) ;define

(define (build-json-string n)
  (let ((out (open-output-string)))
    (display "{" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out)
    ) ;do
    (display "}" out)
    (get-output-string out)
  ) ;let
) ;define

(define (build-array-string n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display i out)
    ) ;do
    (display "]" out)
    (get-output-string out)
  ) ;let
) ;define

(define (build-escape-string n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"a\\nb\\tc\\\\d\\/e\\u4E2D\\u6587\\uD83D\\uDE00\"" out)
    ) ;do
    (display "]" out)
    (get-output-string out)
  ) ;let
) ;define

(define bench-obj-str (build-json-string 200))

(define bench-arr-str (build-array-string 200))

(define bench-esc-str (build-escape-string 50))

(define bench-nested-str
  "{\"user\":{\"id\":1001,\"name\":\"Alice\",\"active\":true,\"email\":null,\"tags\":[\"dev\",\"scheme\",\"json\"],\"profile\":{\"age\":21,\"height\":168.5,\"hobbies\":[\"music\",\"reading\"]}},\"scores\":[98,87,93]}"
) ;define

;; 正确性 sanity check：两种实现输出必须一致
(unless (equal? (string->json bench-obj-str) (string->json/scheme bench-obj-str))
  (error 'value-error "object output mismatch")
) ;unless
(unless (equal? (string->json bench-arr-str) (string->json/scheme bench-arr-str))
  (error 'value-error "array output mismatch")
) ;unless
(unless (equal? (string->json bench-esc-str) (string->json/scheme bench-esc-str))
  (error 'value-error "escape output mismatch")
) ;unless
(unless (equal? (string->json bench-nested-str) (string->json/scheme bench-nested-str))
  (error 'value-error "nested output mismatch")
) ;unless

(define iter 200)

(define (report title time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iter)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

;; warmup
(do ((i 0 (+ i 1)))
  ((= i 20))
  (string->json bench-obj-str)
  (string->json/scheme bench-obj-str)
) ;do

(display "=== string->json C++ vs Scheme 性能对比 ===")
(newline)
(newline)

(let ((t (timeit (lambda () (string->json bench-obj-str)) '() iter)))
  (report "对象/C++" t)
) ;let
(let ((t (timeit (lambda () (string->json/scheme bench-obj-str)) '() iter)))
  (report "对象/Scheme" t)
) ;let
(let ((t (timeit (lambda () (string->json bench-arr-str)) '() iter)))
  (report "数组/C++" t)
) ;let
(let ((t (timeit (lambda () (string->json/scheme bench-arr-str)) '() iter)))
  (report "数组/Scheme" t)
) ;let
(let ((t (timeit (lambda () (string->json bench-esc-str)) '() iter)))
  (report "转义/C++" t)
) ;let
(let ((t (timeit (lambda () (string->json/scheme bench-esc-str)) '() iter)))
  (report "转义/Scheme" t)
) ;let
(let ((t (timeit (lambda () (string->json bench-nested-str)) '() (* 10 iter))))
  (display "[嵌套/C++] iterations=")
  (display (* 10 iter))
  (display " time=")
  (display t)
  (display "s")
  (newline)
) ;let
(let ((t (timeit (lambda () (string->json/scheme bench-nested-str)) '() (* 10 iter)))
     ) ;
  (display "[嵌套/Scheme] iterations=")
  (display (* 10 iter))
  (display " time=")
  (display t)
  (display "s")
  (newline)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)

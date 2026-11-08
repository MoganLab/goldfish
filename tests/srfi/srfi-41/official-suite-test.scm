;; (srfi srfi-41) 官方单元测试移植
;;
;; 来源：SRFI 41 参考实现 r5rs-test.ss（Philip L. Bewig，MIT 许可）
;; 改编：assert 改为 (liii check) 的 check / check-catch。
;;
;; 语法
;; ----
;; 无（测试文件）
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; 无
;;
;; 说明
;; ----
;; 覆盖 SRFI 41 全部导出标识符的官方行为：
;; 流类型判断、构造、变换、stream-match/stream-of 宏，
;; 以及错误分支（non-stream / non-procedural 等参数校验）。

(import (liii check) (srfi srfi-41) (scheme base))

(check-set-mode! 'report-failed)

(define strm123 (stream 1 2 3))

;; ---- stream-null / stream-cons / stream? / stream-null? ----
(check (stream? stream-null) => #t)
(check (stream-null? stream-null) => #t)
(check (stream-pair? stream-null) => #f)
(check (stream? (stream-cons 1 stream-null)) => #t)
(check (stream-null? (stream-cons 1 stream-null)) => #f)
(check (stream-pair? (stream-cons 1 stream-null)) => #t)
(check (stream? "four") => #f)
(check (stream-null? "four") => #f)
(check (stream-pair? "four") => #f)

;; ---- stream-car / stream-cdr ----
(check-catch 'stream-car (stream-car "four"))
(check-catch 'stream-car (stream-car stream-null))
(check (stream-car strm123) => 1)
(check-catch 'stream-cdr (stream-cdr "four"))
(check-catch 'stream-cdr (stream-cdr stream-null))
(check (stream-car (stream-cdr strm123)) => 2)

;; ---- stream-lambda ----
(check
  (stream->list
    (letrec ((double
               (stream-lambda (strm)
                 (if (stream-null? strm)
                   stream-null
                   (stream-cons
                     (* 2 (stream-car strm))
                     (double (stream-cdr strm)))))))
      (double strm123)))
  => '(2 4 6))

;; ---- define-stream ----
(check
  (stream->list
    (let ()
      (define-stream (double strm)
        (if (stream-null? strm)
          stream-null
          (stream-cons
            (* 2 (stream-car strm))
            (double (stream-cdr strm)))))
      (double strm123)))
  => '(2 4 6))

;; ---- list->stream ----
(check-catch 'list->stream (list->stream "four"))
(check (stream->list (list->stream '())) => '())
(check (stream->list (list->stream '(1 2 3))) => '(1 2 3))

;; ---- stream 构造 ----
(check (stream->list (stream)) => '())
(check (stream->list (stream 1)) => '(1))
(check (stream->list (stream 1 2 3)) => '(1 2 3))

;; ---- stream->list ----
(check-catch 'stream->list (stream->list '()))
(check-catch 'stream->list (stream->list "four" strm123))
(check-catch 'stream->list (stream->list -1 strm123))
(check (stream->list (stream)) => '())
(check (stream->list strm123) => '(1 2 3))
(check (stream->list 5 strm123) => '(1 2 3))
(check (stream->list 3 (stream-from 1)) => '(1 2 3))

;; ---- stream-append ----
(check-catch 'stream-append (stream-append "four"))
(check (stream->list (stream-append strm123)) => '(1 2 3))
(check (stream->list (stream-append strm123 strm123)) => '(1 2 3 1 2 3))
(check (stream->list (stream-append strm123 strm123 strm123))
  => '(1 2 3 1 2 3 1 2 3))
(check (stream->list (stream-append strm123 stream-null)) => '(1 2 3))
(check (stream->list (stream-append stream-null strm123)) => '(1 2 3))

;; ---- stream-concat ----
(check-catch 'stream-concat (stream-concat "four"))
(check (stream->list (stream-concat (stream strm123))) => '(1 2 3))
(check (stream->list (stream-concat (stream strm123 strm123)))
  => '(1 2 3 1 2 3))

;; ---- stream-constant ----
(check (stream-ref (stream-constant 1) 100) => 1)
(check (stream-ref (stream-constant 1 2) 100) => 1)
(check (stream-ref (stream-constant 1 2 3) 3) => 1)

;; ---- stream-drop ----
(check-catch 'stream-drop (stream-drop "four" strm123))
(check-catch 'stream-drop (stream-drop -1 strm123))
(check-catch 'stream-drop (stream-drop 2 "four"))
(check (stream->list (stream-drop 0 stream-null)) => '())
(check (stream->list (stream-drop 0 strm123)) => '(1 2 3))
(check (stream->list (stream-drop 1 strm123)) => '(2 3))
(check (stream->list (stream-drop 5 strm123)) => '())

;; ---- stream-drop-while ----
(check-catch 'stream-drop-while (stream-drop-while "four" strm123))
(check-catch 'stream-drop-while (stream-drop-while odd? "four"))
(check (stream->list (stream-drop-while odd? stream-null)) => '())
(check (stream->list (stream-drop-while odd? strm123)) => '(2 3))
(check (stream->list (stream-drop-while even? strm123)) => '(1 2 3))
(check (stream->list (stream-drop-while positive? strm123)) => '())
(check (stream->list (stream-drop-while negative? strm123)) => '(1 2 3))

;; ---- stream-filter ----
(check-catch 'stream-filter (stream-filter "four" strm123))
(check-catch 'stream-filter (stream-filter odd? '()))
(check (stream-null? (stream-filter odd? (stream))) => #t)
(check (stream->list (stream-filter odd? strm123)) => '(1 3))
(check (stream->list (stream-filter even? strm123)) => '(2))
(check (stream->list (stream-filter positive? strm123)) => '(1 2 3))
(check (stream->list (stream-filter negative? strm123)) => '())
(check (odd? (stream-ref (stream-filter odd? (stream-from 0)) 10)) => #t)

;; ---- stream-fold ----
(check-catch 'stream-fold (stream-fold "four" 0 strm123))
(check-catch 'stream-fold (stream-fold + 0 '()))
(check (stream-fold + 0 strm123) => 6)

;; ---- stream-for-each ----
(check-catch 'stream-for-each (stream-for-each "four" strm123))
(check-catch 'stream-for-each (stream-for-each display))
(check-catch 'stream-for-each (stream-for-each display "four"))
(check (let ((sum 0))
          (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123)
          sum)
  => 6)

;; ---- stream-from ----
(check-catch 'stream-from (stream-from "four"))
(check-catch 'stream-from (stream-from 1 "four"))
(check (stream-ref (stream-from 0) 100) => 100)
(check (stream-ref (stream-from 1 2) 100) => 201)
(check (stream-ref (stream-from 0 -1) 100) => -100)

;; ---- stream-iterate ----
(check-catch 'stream-iterate (stream-iterate "four" 0))
(check (stream->list 3 (stream-iterate (lambda (x) (+ x 1)) 1)) => '(1 2 3))

;; ---- stream-length ----
(check-catch 'stream-length (stream-length "four"))
(check (stream-length (stream)) => 0)
(check (stream-length strm123) => 3)

;; ---- stream-let ----
(check
  (stream->list
    (stream-let loop ((strm strm123))
      (if (stream-null? strm)
        stream-null
        (stream-cons
          (* 2 (stream-car strm))
          (loop (stream-cdr strm))))))
  => '(2 4 6))

;; ---- stream-map ----
(check-catch 'stream-map (stream-map "four" strm123))
(check-catch 'stream-map (stream-map odd?))
(check-catch 'stream-map (stream-map odd? "four"))
(check (stream->list (stream-map - strm123)) => '(-1 -2 -3))
(check (stream->list (stream-map + strm123 strm123)) => '(2 4 6))
(check (stream->list (stream-map + strm123 (stream-from 1))) => '(2 4 6))
(check (stream->list (stream-map + (stream-from 1) strm123)) => '(2 4 6))
(check (stream->list (stream-map + strm123 strm123 strm123)) => '(3 6 9))

;; ---- stream-match ----
(check-catch 'stream-match (stream-match '(1 2 3) (_ 'ok)))
(check-catch 'stream-match (stream-match strm123 (() 42)))
(check (stream-match stream-null (() 'ok)) => 'ok)
(check (stream-match strm123 (() 'no) (else 'ok)) => 'ok)
(check (stream-match (stream 1) (() 'no) ((a) a)) => 1)
(check (stream-match (stream 1) (() 'no) ((_) 'ok)) => 'ok)
(check (stream-match strm123 ((a b c) (list a b c))) => '(1 2 3))
(check (stream-match strm123 ((a . _) a)) => 1)
(check (stream-match strm123 ((a b . _) (list a b))) => '(1 2))
(check (stream-match strm123 ((a b . c) (list a b (stream-car c)))) => '(1 2 3))
(check (stream-match strm123 (s (stream->list s))) => '(1 2 3))
(check (stream-match strm123 ((a . _) (= a 1) 'ok)) => 'ok)
(check (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) => 'no)
(check (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) => 'no)
(check (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) => 'yes)

;; ---- stream-of ----
(check
  (stream->list
    (stream-of (+ y 6)
      (x in (stream-range 1 6))
      (odd? x)
      (y is (* x x))))
  => '(7 15 31))
(check
  (stream->list
    (stream-of (* x y)
      (x in (stream-range 1 4))
      (y in (stream-range 1 5))))
  => '(1 2 3 4 2 4 6 8 3 6 9 12))
(check (stream-car (stream-of 1)) => 1)

;; ---- stream-range ----
(check-catch 'stream-range (stream-range "four" 0))
(check-catch 'stream-range (stream-range 0 "four"))
(check-catch 'stream-range (stream-range 1 2 "three"))
(check (stream->list (stream-range 0 5)) => '(0 1 2 3 4))
(check (stream->list (stream-range 5 0)) => '(5 4 3 2 1))
(check (stream->list (stream-range 0 5 2)) => '(0 2 4))
(check (stream->list (stream-range 5 0 -2)) => '(5 3 1))
(check (stream->list (stream-range 0 1 -1)) => '())

;; ---- stream-ref ----
(check-catch 'stream-ref (stream-ref '() 4))
(check-catch 'stream-ref (stream-ref (stream-from 0) 3.5))
(check-catch 'stream-ref (stream-ref (stream-from 0) -3))
(check-catch 'stream-ref (stream-ref strm123 5))
(check (stream-ref strm123 0) => 1)
(check (stream-ref strm123 1) => 2)
(check (stream-ref strm123 2) => 3)

;; ---- stream-reverse ----
(check-catch 'stream-reverse (stream-reverse '()))
(check (stream->list (stream-reverse (stream))) => '())
(check (stream->list (stream-reverse strm123)) => '(3 2 1))

;; ---- stream-scan ----
(check-catch 'stream-scan (stream-scan "four" 0 strm123))
(check-catch 'stream-scan (stream-scan + 0 '()))
(check (stream->list (stream-scan + 0 strm123)) => '(0 1 3 6))

;; ---- stream-take ----
(check-catch 'stream-take (stream-take 5 "four"))
(check-catch 'stream-take (stream-take "four" strm123))
(check-catch 'stream-take (stream-take -4 strm123))
(check (stream->list (stream-take 5 stream-null)) => '())
(check (stream->list (stream-take 0 stream-null)) => '())
(check (stream->list (stream-take 0 strm123)) => '())
(check (stream->list (stream-take 2 strm123)) => '(1 2))
(check (stream->list (stream-take 3 strm123)) => '(1 2 3))
(check (stream->list (stream-take 5 strm123)) => '(1 2 3))

;; ---- stream-take-while ----
(check-catch 'stream-take-while (stream-take-while odd? "four"))
(check-catch 'stream-take-while (stream-take-while "four" strm123))
(check (stream->list (stream-take-while odd? strm123)) => '(1))
(check (stream->list (stream-take-while even? strm123)) => '())
(check (stream->list (stream-take-while positive? strm123)) => '(1 2 3))
(check (stream->list (stream-take-while negative? strm123)) => '())

;; ---- stream-unfold ----
(check-catch 'stream-unfold (stream-unfold "four" odd? + 0))
(check-catch 'stream-unfold (stream-unfold + "four" + 0))
(check-catch 'stream-unfold (stream-unfold + odd? "four" 0))
(check
  (stream->list
    (stream-unfold
      (lambda (x) (* x x))
      (lambda (x) (< x 10))
      (lambda (x) (+ x 1))
      0))
  => '(0 1 4 9 16 25 36 49 64 81))

;; ---- stream-unfolds ----
(check
  (stream->list
    (stream-unfolds
      (lambda (x)
        (let ((n (car x)) (s (cdr x)))
          (if (zero? n)
            (values 'dummy '())
            (values
              (cons (- n 1) (stream-cdr s))
              (list (stream-car s))))))
      (cons 5 (stream-from 0))))
  => '(0 1 2 3 4))

;; ---- stream-zip ----
(check-catch 'stream-zip (stream-zip))
(check-catch 'stream-zip (stream-zip "four"))
(check-catch 'stream-zip (stream-zip strm123 "four"))
(check (stream->list (stream-zip strm123 stream-null)) => '())
(check (stream->list (stream-zip strm123)) => '((1) (2) (3)))
(check (stream->list (stream-zip strm123 strm123)) => '((1 1) (2 2) (3 3)))
(check (stream->list (stream-zip strm123 (stream-from 1)))
  => '((1 1) (2 2) (3 3)))
(check (stream->list (stream-zip strm123 strm123 strm123))
  => '((1 1 1) (2 2 2) (3 3 3)))

(check-report)

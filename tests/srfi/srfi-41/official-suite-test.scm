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

;;
;; 错误处理
;; ----
;; 无

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
(check-catch 'type-error (stream-car "four"))
(check-catch 'value-error (stream-car stream-null))
(check (stream-car strm123) => 1)
(check-catch 'type-error (stream-cdr "four"))
(check-catch 'value-error (stream-cdr stream-null))
(check (stream-car (stream-cdr strm123)) => 2)

;; ---- stream-lambda ----
(check
  (stream->list
    (letrec ((double
               (stream-lambda (strm)
                 (if (stream-null? strm)
                   stream-null
                   (stream-cons (* 2 (stream-car strm)) (double (stream-cdr strm)))
                 ) ;if
               ) ;stream-lambda
             ) ;double
            ) ;
      (double strm123)
    ) ;letrec
  ) ;stream->list
  =>
  '(2 4 6)
) ;check

;; ---- define-stream ----
(check
  (stream->list
    (let ()
      (define-stream (double strm)
        (if (stream-null? strm)
          stream-null
          (stream-cons (* 2 (stream-car strm)) (double (stream-cdr strm)))
        ) ;if
      ) ;define-stream
      (double strm123)
    ) ;let
  ) ;stream->list
  =>
  '(2 4 6)
) ;check

;; ---- list->stream ----
(check-catch 'type-error (list->stream "four"))
(check (stream->list (list->stream '())) => '())
(check (stream->list (list->stream '(1 2 3))) => '(1 2 3))

;; ---- stream 构造 ----
(check (stream->list (stream)) => '())
(check (stream->list (stream 1)) => '(1))
(check (stream->list (stream 1 2 3)) => '(1 2 3))

;; ---- stream->list ----
(check-catch 'type-error (stream->list '()))
(check-catch 'type-error (stream->list "four" strm123))
(check-catch 'value-error (stream->list -1 strm123))
(check (stream->list (stream)) => '())
(check (stream->list strm123) => '(1 2 3))
(check (stream->list 5 strm123) => '(1 2 3))
(check (stream->list 3 (stream-from 1)) => '(1 2 3))

;; ---- stream-append ----
(check-catch 'type-error (stream-append "four"))
(check (stream->list (stream-append strm123)) => '(1 2 3))
(check (stream->list (stream-append strm123 strm123)) => '(1 2 3 1 2 3))
(check (stream->list (stream-append strm123 strm123 strm123))
  =>
  '(1 2 3 1 2 3 1 2 3)
) ;check
(check (stream->list (stream-append strm123 stream-null)) => '(1 2 3))
(check (stream->list (stream-append stream-null strm123)) => '(1 2 3))

;; ---- stream-concat ----
(check-catch 'type-error (stream-concat "four"))
(check (stream->list (stream-concat (stream strm123))) => '(1 2 3))
(check (stream->list (stream-concat (stream strm123 strm123)))
  =>
  '(1 2 3 1 2 3)
) ;check

;; ---- stream-constant ----
(check (stream-ref (stream-constant 1) 100) => 1)
(check (stream-ref (stream-constant 1 2) 100) => 1)
(check (stream-ref (stream-constant 1 2 3) 3) => 1)

;; ---- stream-drop ----
(check-catch 'type-error (stream-drop "four" strm123))
(check-catch 'value-error (stream-drop -1 strm123))
(check-catch 'type-error (stream-drop 2 "four"))
(check (stream->list (stream-drop 0 stream-null)) => '())
(check (stream->list (stream-drop 0 strm123)) => '(1 2 3))
(check (stream->list (stream-drop 1 strm123)) => '(2 3))
(check (stream->list (stream-drop 5 strm123)) => '())

;; ---- stream-drop-while ----
(check-catch 'type-error (stream-drop-while "four" strm123))
(check-catch 'type-error (stream-drop-while odd? "four"))
(check (stream->list (stream-drop-while odd? stream-null)) => '())
(check (stream->list (stream-drop-while odd? strm123)) => '(2 3))
(check (stream->list (stream-drop-while even? strm123)) => '(1 2 3))
(check (stream->list (stream-drop-while positive? strm123)) => '())
(check (stream->list (stream-drop-while negative? strm123)) => '(1 2 3))

;; ---- stream-filter ----
(check-catch 'type-error (stream-filter "four" strm123))
(check-catch 'type-error (stream-filter odd? '()))
(check (stream-null? (stream-filter odd? (stream))) => #t)
(check (stream->list (stream-filter odd? strm123)) => '(1 3))
(check (stream->list (stream-filter even? strm123)) => '(2))
(check (stream->list (stream-filter positive? strm123)) => '(1 2 3))
(check (stream->list (stream-filter negative? strm123)) => '())
(check
  (odd? (stream-ref (stream-filter odd? (stream-from 0)) 10))
  =>
  #t
) ;check

;; ---- stream-fold ----
(check-catch 'type-error (stream-fold "four" 0 strm123))
(check-catch 'type-error (stream-fold + 0 '()))
(check (stream-fold + 0 strm123) => 6)

;; ---- stream-for-each ----
(check-catch 'type-error (stream-for-each "four" strm123))
(check-catch 'value-error (stream-for-each display))
(check-catch 'type-error (stream-for-each display "four"))
(check
  (let ((sum 0))
    (stream-for-each (lambda (x) (set! sum (+ sum x))) strm123)
    sum
  ) ;let
  =>
  6
) ;check

;; ---- stream-from ----
(check-catch 'type-error (stream-from "four"))
(check-catch 'type-error (stream-from 1 "four"))
(check (stream-ref (stream-from 0) 100) => 100)
(check (stream-ref (stream-from 1 2) 100) => 201)
(check (stream-ref (stream-from 0 -1) 100) => -100)

;; ---- stream-iterate ----
(check-catch 'type-error (stream-iterate "four" 0))
(check
  (stream->list 3 (stream-iterate (lambda (x) (+ x 1)) 1))
  =>
  '(1 2 3)
) ;check

;; ---- stream-length ----
(check-catch 'type-error (stream-length "four"))
(check (stream-length (stream)) => 0)
(check (stream-length strm123) => 3)

;; ---- stream-let ----
(check
  (stream->list
    (stream-let loop
     ((strm strm123))
     (if (stream-null? strm)
       stream-null
       (stream-cons (* 2 (stream-car strm)) (loop (stream-cdr strm)))
     ) ;if
    ) ;stream-let
  ) ;stream->list
  =>
  '(2 4 6)
) ;check

;; ---- stream-map ----
(check-catch 'type-error (stream-map "four" strm123))
(check-catch 'value-error (stream-map odd?))
(check-catch 'type-error (stream-map odd? "four"))
(check (stream->list (stream-map - strm123)) => '(-1 -2 -3))
(check (stream->list (stream-map + strm123 strm123)) => '(2 4 6))
(check (stream->list (stream-map + strm123 (stream-from 1))) => '(2 4 6))
(check (stream->list (stream-map + (stream-from 1) strm123)) => '(2 4 6))
(check (stream->list (stream-map + strm123 strm123 strm123)) => '(3 6 9))

;; ---- stream-match ----
(check-catch 'type-error (stream-match '(1 2 3) (_ 'ok)))
(check-catch 'value-error (stream-match strm123 (() 42)))
(check (stream-match stream-null (() 'ok)) => 'ok)
(check (stream-match strm123 (() 'no) (else 'ok)) => 'ok)
(check (stream-match (stream 1) (() 'no) ((a) a)) => 1)
(check (stream-match (stream 1) (() 'no) ((_) 'ok)) => 'ok)
(check (stream-match strm123 ((a b c) (list a b c))) => '(1 2 3))
(check (stream-match strm123 ((a . _) a)) => 1)
(check (stream-match strm123 ((a b . _) (list a b))) => '(1 2))
(check
  (stream-match strm123 ((a b . c) (list a b (stream-car c))))
  =>
  '(1 2 3)
) ;check
(check (stream-match strm123 (s (stream->list s))) => '(1 2 3))
(check (stream-match strm123 ((a . _) (= a 1) 'ok)) => 'ok)
(check (stream-match strm123 ((a . _) (= a 2) 'yes) (_ 'no)) => 'no)
(check (stream-match strm123 ((a b c) (= a b) 'yes) (_ 'no)) => 'no)
(check (stream-match (stream 1 1 2) ((a b c) (= a b) 'yes) (_ 'no)) => 'yes)

;; ---- stream-of ----
(check
  (stream->list (stream-of (+ y 6) (x in (stream-range 1 6)) (odd? x) (y is (* x x)))
  ) ;stream->list
  =>
  '(7 15 31)
) ;check
(check
  (stream->list (stream-of (* x y) (x in (stream-range 1 4)) (y in (stream-range 1 5)))
  ) ;stream->list
  =>
  '(1 2 3 4 2 4 6 8 3 6 9 12)
) ;check
(check (stream-car (stream-of 1)) => 1)

;; ---- stream-range ----
(check-catch 'type-error (stream-range "four" 0))
(check-catch 'type-error (stream-range 0 "four"))
(check-catch 'type-error (stream-range 1 2 "three"))
(check (stream->list (stream-range 0 5)) => '(0 1 2 3 4))
(check (stream->list (stream-range 5 0)) => '(5 4 3 2 1))
(check (stream->list (stream-range 0 5 2)) => '(0 2 4))
(check (stream->list (stream-range 5 0 -2)) => '(5 3 1))
(check (stream->list (stream-range 0 1 -1)) => '())

;; ---- stream-ref ----
(check-catch 'type-error (stream-ref '() 4))
(check-catch 'type-error (stream-ref (stream-from 0) 3.5))
(check-catch 'value-error (stream-ref (stream-from 0) -3))
(check-catch 'value-error (stream-ref strm123 5))
(check (stream-ref strm123 0) => 1)
(check (stream-ref strm123 1) => 2)
(check (stream-ref strm123 2) => 3)

;; ---- stream-reverse ----
(check-catch 'type-error (stream-reverse '()))
(check (stream->list (stream-reverse (stream))) => '())
(check (stream->list (stream-reverse strm123)) => '(3 2 1))

;; ---- stream-scan ----
(check-catch 'type-error (stream-scan "four" 0 strm123))
(check-catch 'type-error (stream-scan + 0 '()))
(check (stream->list (stream-scan + 0 strm123)) => '(0 1 3 6))

;; ---- stream-take ----
(check-catch 'type-error (stream-take 5 "four"))
(check-catch 'type-error (stream-take "four" strm123))
(check-catch 'value-error (stream-take -4 strm123))
(check (stream->list (stream-take 5 stream-null)) => '())
(check (stream->list (stream-take 0 stream-null)) => '())
(check (stream->list (stream-take 0 strm123)) => '())
(check (stream->list (stream-take 2 strm123)) => '(1 2))
(check (stream->list (stream-take 3 strm123)) => '(1 2 3))
(check (stream->list (stream-take 5 strm123)) => '(1 2 3))

;; ---- stream-take-while ----
(check-catch 'type-error (stream-take-while odd? "four"))
(check-catch 'type-error (stream-take-while "four" strm123))
(check (stream->list (stream-take-while odd? strm123)) => '(1))
(check (stream->list (stream-take-while even? strm123)) => '())
(check (stream->list (stream-take-while positive? strm123)) => '(1 2 3))
(check (stream->list (stream-take-while negative? strm123)) => '())

;; ---- stream-unfold ----
(check-catch 'type-error (stream-unfold "four" odd? + 0))
(check-catch 'type-error (stream-unfold + "four" + 0))
(check-catch 'type-error (stream-unfold + odd? "four" 0))
(check
  (stream->list (stream-unfold (lambda (x) (* x x))
                  (lambda (x) (< x 10))
                  (lambda (x) (+ x 1))
                  0
                ) ;stream-unfold
  ) ;stream->list
  =>
  '(0 1 4 9 16 25 36 49 64 81)
) ;check

;; ---- stream-unfolds ----
(check
  (stream->list
    (stream-unfolds
      (lambda (x)
        (let ((n (car x)) (s (cdr x)))
          (if (zero? n)
            (values 'dummy '())
            (values (cons (- n 1) (stream-cdr s)) (list (stream-car s)))
          ) ;if
        ) ;let
      ) ;lambda
      (cons 5 (stream-from 0))
    ) ;stream-unfolds
  ) ;stream->list
  =>
  '(0 1 2 3 4)
) ;check

;; ---- stream-zip ----
(check-catch 'value-error (stream-zip))
(check-catch 'type-error (stream-zip "four"))
(check-catch 'type-error (stream-zip strm123 "four"))
(check (stream->list (stream-zip strm123 stream-null)) => '())
(check (stream->list (stream-zip strm123)) => '((1) (2) (3)))
(check (stream->list (stream-zip strm123 strm123)) => '((1 1) (2 2) (3 3)))
(check (stream->list (stream-zip strm123 (stream-from 1)))
  =>
  '((1 1) (2 2) (3 3))
) ;check
(check (stream->list (stream-zip strm123 strm123 strm123))
  =>
  '((1 1 1) (2 2 2) (3 3 3))
) ;check

(check-report)

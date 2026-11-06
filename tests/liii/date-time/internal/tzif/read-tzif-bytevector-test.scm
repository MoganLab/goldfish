;; read-tzif-bytevector
;; 解析 TZif 字节向量为时区转移数据。
;;
;; 语法
;; ----
;; (read-tzif-bytevector bv)
;;
;; 参数
;; ----
;; bv : bytevector? 以 "TZif" 魔数开头的 TZif 数据。
;;
;; 返回值
;; ----
;; values 首个转移（tzif-transition? 或 #f）、转移向量（vector?）与 POSIX 规则（tz-string? 或 #f）。
;;
;; 错误处理
;; --------
;; type-error 当魔数非法时抛出错误。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(define (u8-list->bv l)
  (let ((bv (make-bytevector (length l) 0)))
    (let loop
      ((i 0) (rem l))
      (if (null? rem)
        bv
        (begin
          (bytevector-u8-set! bv i (car rem))
          (loop (+ i 1) (cdr rem))
        ) ;begin
      ) ;if
    ) ;let
  ) ;let
) ;define

(define tzif-bv
  (u8-list->bv
    (append (map char->integer (string->list "TZif"))
      (make-list 16 0)
      (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 4)
      (list 0 0 0 0)
      (list 0)
      (list 0 0 14 16 1 0)
      (append (map char->integer (string->list "EST")) (list 0))
    ) ;append
  ) ;u8-list->bv
) ;define

(call-with-values (lambda () (read-tzif-bytevector tzif-bv))
  (lambda (first transitions footer)
    (check (tzif-transition-offset first) => 3600)
    (check (tzif-transition-dst? first) => #t)
    (check (tzif-transition-designation first) => "EST")
    (check (vector-length transitions) => 1)
    (check (tzif-transition-time (vector-ref transitions 0)) => 0)
    (check footer => #f)
  ) ;lambda
) ;call-with-values

(check-catch 'type-error (read-tzif-bytevector (u8-list->bv '(1 2 3 4 5))))

(check-report)

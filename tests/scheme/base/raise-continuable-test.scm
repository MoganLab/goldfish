(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; raise-continuable
;; 抛出可续延异常。
;;
;; 语法
;; ----
;; (raise-continuable obj)
;;
;; 说明
;; ----
;; Goldfish 采用 R7RS 允许的中止语义（与 raise 相同）。

;; 可由 with-exception-handler 捕获
(check (with-exception-handler
         (lambda (e) (list 'handled e))
         (lambda () (raise-continuable 'oops)))
       => '(handled oops))

;; 抛出对象原样传递
(check (with-exception-handler
         (lambda (e) e)
         (lambda () (raise-continuable 42)))
       => 42)

;; 可由 guard 捕获
(check (guard (e (else (list 'guarded e)))
         (raise-continuable 'boom))
       => '(guarded boom))

(check-report)

;; current-input-port
;; 返回当前的输入端口。
;;
;; 语法
;; ----
;; (current-input-port)
;;
;; 返回值
;; ------
;; input-port?
;; 当前默认的输入端口。
;;
;; 说明
;; ----
;; 1. 通常是标准输入
;; 2. 可被 with-input-from-file 临时改变

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (input-port? (current-input-port)) => #t)
(check (eq? (current-input-port) *stdin*) => #t)

(check
  (with-input-from-file "tests/scheme/base/current-input-port-test.scm"
    (lambda ()
      (list (input-port? (current-input-port))
        (not (eq? (current-input-port) *stdin*))
        (read-char)
      ) ;list
    ) ;lambda
  ) ;with-input-from-file
  =>
  '(#t #t #\;)
) ;check

(check (eq? (current-input-port) *stdin*) => #t)

(let ((orig (current-input-port)) (p (open-input-string "test")))
  (set-current-input-port p)
  (check (eq? (current-input-port) p) => #t)
  (check (read-char) => #\t)
  (set-current-input-port orig)
  (check (eq? (current-input-port) orig) => #t)
) ;let

(check-report)

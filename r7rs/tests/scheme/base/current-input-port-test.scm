;; current-input-port
;; 返回当前的输入端口。
;;
;;
;; 语法
;; ----
;; (current-input-port)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; input-port?
;; 当前默认的输入端口。
;;
;; 说明
;; ----
;; 1. 通常是标准输入
;; 2. 可被 with-input-from-file 临时改变

(import (liii check) (liii tempfile))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (input-port? (current-input-port)) => #t)
(check (eq? (current-input-port) *stdin*) => #t)

(with-temp-file
  (lambda (tmp-file)
    (let ((out (open-output-file tmp-file)))
      (write-string ";hello" out)
      (close-output-port out)
    ) ;let
    (check
      (with-input-from-file tmp-file
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
  ) ;lambda
) ;with-temp-file

(check (eq? (current-input-port) *stdin*) => #t)

(let ((orig (current-input-port)) (p (open-input-string "test")))
  (set-current-input-port p)
  (check (eq? (current-input-port) p) => #t)
  (check (read-char) => #\t)
  (set-current-input-port orig)
  (check (eq? (current-input-port) orig) => #t)
) ;let

(check-report)

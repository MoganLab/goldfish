;; with-input-from-file
;; 临时将输入重定向到文件。
;;
;;
;; 语法
;; ----
;; (with-input-from-file string thunk)
;;
;; 参数
;; ----
;; string : string?
;; 要读取的文件名。
;; thunk : procedure?
;; 无参数过程，在重定向环境中执行。
;;
;; 返回值
;; ----
;; 任意类型
;; thunk 的返回值。
;;
;; 说明
;; ----
;; 1. thunk 执行期间当前输入端口指向该文件
;; 2. 执行结束后恢复原来的输入端口

(import (liii check) (liii tempfile))
(import (scheme base))

(check-set-mode! 'report-failed)

(with-temp-file
  (lambda (tmp-file)
    (let ((out (open-output-file tmp-file)))
      (write-string ";hello" out)
      (close-output-port out)
    ) ;let
    (check (with-input-from-file tmp-file (lambda () (read-char))) => #\;)
  ) ;lambda
) ;with-temp-file

(check-report)

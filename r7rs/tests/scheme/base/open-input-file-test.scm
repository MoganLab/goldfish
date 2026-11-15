;; open-input-file
;; 打开文件作为输入端口。
;;
;;
;; 语法
;; ----
;; (open-input-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要打开的文件名。
;;
;; 返回值
;; ----
;; input-port?
;; 文件输入端口。
;;
;; 说明
;; ----
;; 1. 文件必须存在且可读
;; 2. 使用后应关闭端口
;; 3. 返回二进制端口

(import (liii check) (liii path) (liii tempfile))
(import (scheme base))

(check-set-mode! 'report-failed)

(with-temp-dir
  (lambda (dir)
    (let ((tmp-file (path->string (path-join dir "test-open-input.scm"))))
      (let ((out (open-output-file tmp-file)))
        (write-string ";hello" out)
        (close-output-port out)
      ) ;let
      (let ((p (open-input-file tmp-file)))
        (check (input-port? p) => #t)
        (close-input-port p)
      ) ;let
      (check-catch 'io-error
        (open-input-file (path->string (path-join dir "nonexistent-file-12345.scm")))
      ) ;check-catch
    ) ;let
  ) ;lambda
) ;with-temp-dir

(check-report)

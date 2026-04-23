(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; open-input-file
;; 打开文件作为输入端口。
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
;; ------
;; input-port?
;; 文件输入端口。
;;
;; 说明
;; ----
;; 1. 文件必须存在且可读
;; 2. 使用后应关闭端口
;; 3. 返回二进制端口
(let ((p (open-input-file "tests/scheme/base/apply-test.scm")))
  (check (input-port? p) => #t)
  (close-input-port p)
) ;let
(check-catch 'io-error
  (open-input-file "tests/scheme/base/nonexistent-file-12345.scm")
) ;check-catch

(check-report)

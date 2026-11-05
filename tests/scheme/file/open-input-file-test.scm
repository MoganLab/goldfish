;; open-input-file
;; 打开文件作为文本输入端口。
;;
;; 语法
;; ----
;; (open-input-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要打开的文件路径。
;;
;; 返回值
;; ----
;; input-port?
;; 打开的输入端口。
;;
;; 说明
;; ----
;; 1. 文件必须存在且可读。
;; 2. 使用完毕后应调用 close-port 或 close-input-port 关闭端口。

(import (liii check) (scheme file))
(check-set-mode! 'report-failed)

(define test-file "tests/scheme/file/test-open-input.txt")
;; 创建测试文件
(with-output-to-file test-file
  (lambda () (display "(line1 line2)\n") (display "line3"))
) ;with-output-to-file
;; 测试 open-input-file
;; 文件端口必须记录真实文件名，供 pair-filename、异常定位和 stack trace 使用。
;; 不能让 file-number 保持为 0，否则 pair-filename 会错误返回 *stdout* 等无关文件。
(let* ((port (open-input-file test-file))
       (form (read port)))
  (check (pair-filename form) => test-file)
  (check (pair-line-number form) => 1)
  (close-port port)
) ;let*
;; 测试 open-input-file
(let ((port (open-input-file test-file)))
  (check-true (input-port? port))
  (check (read-line port) => "(line1 line2)")
  (check (read-line port) => "line3")
  (close-port port)
) ;let
;; 测试中文文件名

(define chinese-file "tests/scheme/file/中文输入.txt")
(with-output-to-file chinese-file
  (lambda () (display "第一行\n") (display "第二行"))
) ;with-output-to-file
(let ((port (open-input-file chinese-file)))
  (check-true (input-port? port))
  (check (read-line port) => "第一行")
  (check (read-line port) => "第二行")
  (close-port port)
) ;let
(delete-file chinese-file)
;; 清理
(delete-file test-file)

(check-report)

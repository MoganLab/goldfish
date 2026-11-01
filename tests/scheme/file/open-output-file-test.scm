;; open-output-file
;; 打开文件作为文本输出端口。
;;
;; 语法
;; ----
;; (open-output-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要打开或创建的文件路径。
;;
;; 返回值
;; ----
;; output-port?
;; 打开的输出端口。
;;
;; 说明
;; ----
;; 1. 若文件已存在则覆盖原内容。
;; 2. 使用完毕后应调用 close-port 或 close-output-port 关闭端口。

(import (liii check) (scheme file))
(check-set-mode! 'report-failed)

(define test-file "tests/scheme/file/test-open-output.txt")
;; 测试 open-output-file
(let ((port (open-output-file test-file)))
  (check-true (output-port? port))
  (display "output content" port)
  (close-port port)
) ;let
;; 验证内容
(check (call-with-input-file test-file (lambda (port) (read-string 100 port)))
  =>
  "output content"
) ;check
;; 测试覆盖写入
(let ((port (open-output-file test-file)))
  (display "new content" port)
  (close-port port)
) ;let
(check (call-with-input-file test-file (lambda (port) (read-string 100 port)))
  =>
  "new content"
) ;check
;; 测试中文文件名

(define chinese-file "tests/scheme/file/中文输出文件.txt")
(let ((port (open-output-file chinese-file)))
  (check-true (output-port? port))
  (display "中文输出内容" port)
  (close-port port)
) ;let
(check (call-with-input-file chinese-file (lambda (port) (read-string 100 port)))
  =>
  "中文输出内容"
) ;check
(delete-file chinese-file)
;; 清理
(delete-file test-file)

(check-report)

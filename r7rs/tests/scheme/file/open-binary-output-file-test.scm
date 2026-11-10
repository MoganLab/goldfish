;; open-binary-output-file
;; 打开文件作为二进制输出端口。
;;
;; 语法
;; ----
;; (open-binary-output-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要打开或创建的文件路径。
;;
;; 返回值
;; ----
;; binary-port?
;; 打开的二进制输出端口。
;;
;; 说明
;; ----
;; 1. 若文件已存在则覆盖原内容。
;; 2. 使用完毕后应调用 close-port 关闭端口。

(import (liii check) (scheme base) (scheme file))
(check-set-mode! 'report-failed)

(define base-dir
  (if (file-exists? "r7rs/tests/scheme/file")
    "r7rs/tests/scheme/file/"
    "tests/scheme/file/"
  ) ;if
) ;define

(define test-file (string-append base-dir "test-binary-output.bin"))
;; 测试 open-binary-output-file 写入二进制数据
(let ((port (open-binary-output-file test-file)))
  (check-true (output-port? port))
  (check-true (binary-port? port))
  (write-u8 0 port)
  (write-bytevector (bytevector 1 2 255) port)
  (close-port port)
) ;let
;; 验证内容
(let ((port (open-binary-input-file test-file)))
  (check (read-bytevector 10 port) => (bytevector 0 1 2 255))
  (close-port port)
) ;let
;; 测试中文文件名

(define chinese-file (string-append base-dir "中文二进制输出.bin"))
(let ((port (open-binary-output-file chinese-file)))
  (check-true (output-port? port))
  (check-true (binary-port? port))
  (display "中文输出" port)
  (close-port port)
) ;let
(let ((port (open-binary-input-file chinese-file)))
  (check (read-string 100 port) => "中文输出")
  (close-port port)
) ;let
(delete-file chinese-file)
;; 清理
(delete-file test-file)

(check-report)

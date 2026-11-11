;; open-binary-input-file
;; 打开文件作为二进制输入端口。
;;
;; 语法
;; ----
;; (open-binary-input-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要打开的文件路径。
;;
;; 返回值
;; ----
;; binary-port?
;; 打开的二进制输入端口。
;;
;; 说明
;; ----
;; 1. 文件必须存在且可读。
;; 2. 使用完毕后应调用 close-port 关闭端口。

(import (liii check) (scheme base) (scheme file))
(check-set-mode! 'report-failed)

(define base-dir
  (if (file-exists? "r7rs/tests/scheme/file")
    "r7rs/tests/scheme/file/"
    "tests/scheme/file/"
  ) ;if
) ;define

(define test-file (string-append base-dir "test-binary-input.bin"))
;; 创建测试文件（二进制内容）
(let ((out (open-binary-output-file test-file)))
  (write-bytevector (bytevector 0 127 128 255 65) out)
  (close-port out)
) ;let
;; 测试 open-binary-input-file 与二进制读取
(let ((port (open-binary-input-file test-file)))
  (check-true (input-port? port))
  (check-true (binary-port? port))
  (check (read-u8 port) => 0)
  (check (peek-u8 port) => 127)
  (check (read-bytevector 3 port) => (bytevector 127 128 255))
  (check (read-u8 port) => 65)
  (check (eof-object? (read-u8 port)) => #t)
  (close-port port)
) ;let
;; 测试中文文件名

(define chinese-file (string-append base-dir "中文二进制输入.bin"))
(let ((out (open-binary-output-file chinese-file)))
  (write-bytevector (string->utf8 "中文二进制内容") out)
  (close-port out)
) ;let
(let ((port (open-binary-input-file chinese-file)))
  (check-true (input-port? port))
  (check-true (binary-port? port))
  (check (utf8->string (read-bytevector 100 port)) => "中文二进制内容")
  ;; 验证端口隔离：二进制端口不能使用文本读取过程
  (check-catch 'type-error (read-string 10 port))
  (close-port port)
) ;let
(delete-file chinese-file)
;; 清理
(delete-file test-file)

(check-report)

(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; open-input-string
;; 将一个字符串转换为输入端口
;;
;; 语法
;; ----
;; (open-input-string string)
;;
;; 参数
;; ----
;; string : string?
;; 一个字符串对象
;;
;; 返回值
;; -----
;; port
;; 一个文本输入端口，该端口会从给定的字符串中读取字符。
;; 注意：如果在端口使用期间修改了原始字符串，其行为是未定义的。
;; eof on empty
(let ((port (open-input-string "")))
  (check (eof-object? (read-char port))
    =>
    #t
  ) ;check
) ;let
;; read-char
(let ((port (open-input-string "abc")))
  (check (read-char port) => #\a)
  (check (read-char port) => #\b)
  (check (read-char port) => #\c)
  (check (eof-object? (read-char port))
    =>
    #t
  ) ;check
) ;let
;; read-char, Unicode (Not Support)
(let ((port (open-input-string "λμ")))
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbb)
  (check (read-char port) => #\xce)
  (check (read-char port) => #\xbc)
) ;let
;; read-string, Unicode
(let ((port (open-input-string "λμ")))
  (check (read-string 2 port) => "λ")
  (check (read-string 2 port) => "μ")
) ;let
(check-report)

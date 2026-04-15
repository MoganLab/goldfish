(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-upcase
;; 将字符转换为大写形式
;;
;; 语法
;; ----
;; (char-upcase char) → char
;;
;; 参数
;; ----
;; char : character
;; 要转换的字符
;;
;; 返回值
;; ------
;; character
;; 转换后的大写字符
;;
;; 注意
;; ----
;; - 对于小写字母，返回对应的大写字母
;; - 对于大写字母，返回原字符
;; - 对于非字母字符，返回原字符
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
(check (char-upcase #\z) => #\Z)
(check (char-upcase #\a) => #\A)
(check (char-upcase #\A) => #\A)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\$) => #\$)
(check (char-upcase #\.) => #\.)
(check (char-upcase #\\) => #\\)
(check (char-upcase #\5) => #\5)
(check (char-upcase #\)) => #\))
(check (char-upcase #\%) => #\%)
(check (char-upcase #\0) => #\0)
(check (char-upcase #\_) => #\_)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\space) => #\space)
(check (char-upcase #\newline) => #\newline)
(check (char-upcase #\null) => #\null)
;; 错误处理测试
(check-catch 'type-error (char-upcase "a"))
(check-catch 'type-error (char-upcase 65))
(check-catch 'type-error (char-upcase 'a))
(check-report)
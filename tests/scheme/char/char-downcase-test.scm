(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-downcase
;; 将字符转换为小写形式
;;
;; 语法
;; ----
;; (char-downcase char) → char
;;
;; 参数
;; ----
;; char : character
;; 要转换的字符
;;
;; 返回值
;; ------
;; character
;; 转换后的小写字符
;;
;; 注意
;; ----
;; - 对于大写字母，返回对应的小写字母
;; - 对于小写字母，返回原字符
;; - 对于非字母字符，返回原字符
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
(check (char-downcase #\A) => #\a)
(check (char-downcase #\Z) => #\z)
(check (char-downcase #\a) => #\a)
;; 错误处理测试
(check-catch 'type-error (char-downcase "A"))
(check-catch 'type-error (char-downcase 65))
(check-catch 'type-error (char-downcase 'A))
(check-report)
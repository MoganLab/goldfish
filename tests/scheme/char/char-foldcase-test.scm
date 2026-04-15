(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; char-foldcase
;; 执行字符的大小写折叠
;;
;; 语法
;; ----
;; (char-foldcase char) → char
;;
;; 参数
;; ----
;; char : character
;; 要转换的字符
;;
;; 返回值
;; ------
;; character
;; 转换后的字符
;;
;; 注意
;; ----
;; - 对于 ASCII 字母字符，当前实现与 char-downcase 相同
;; - 对于非字母字符，返回原字符
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符类型，否则会抛出异常
;; 基本测试 - char-foldcase 当前实现与 char-downcase 相同
(check (char-foldcase #\A) => #\a)
(check (char-foldcase #\Z) => #\z)
(check (char-foldcase #\a) => #\a)
(check (char-foldcase #\z) => #\z)
;; 非字母字符测试
(check (char-foldcase #\5) => #\5)
(check (char-foldcase #\space)
  =>
  #\space
) ;check
(check (char-foldcase #\!) => #\!)
;; 错误处理测试
(check-catch 'type-error
  (char-foldcase "A")
) ;check-catch
(check-catch 'type-error
  (char-foldcase 65)
) ;check-catch
(check-catch 'type-error
  (char-foldcase 'A)
) ;check-catch
(check-report)
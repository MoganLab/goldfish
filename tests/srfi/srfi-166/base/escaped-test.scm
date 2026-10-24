(import (liii check)
        (scheme char)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; escaped
;; 输出转义字符串的组合子。支持自定义引号、转义字符和重命名规则。
;;
;; 语法
;; ----
;; (escaped fmt [quot] [esc] [rename])
;; (maybe-escaped fmt pred [quot] [esc] [rename])
;;
;; 示例
;; ----
;; (show #f (escaped "hi, bob!")) => "hi, bob!"
;; (show #f (escaped "hi, 'bob'" #\')) => "hi, \\'bob\\'"

(check (show #f (escaped "hi, bob!")) => "hi, bob!")
(check (show #f (escaped "hi, 'bob'" #\')) => "hi, \\'bob\\'")
(check (show #f (escaped "hi, 'bob'" #\' #\')) => "hi, ''bob''")
(check (show #f (maybe-escaped "bob" char-whitespace?)) => "bob")
(check (show #f (maybe-escaped "hi, bob!" char-whitespace?)) => "\"hi, bob!\"")

(check-report)

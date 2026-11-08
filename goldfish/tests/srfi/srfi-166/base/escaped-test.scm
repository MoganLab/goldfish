;; escaped
;; 按指定引号与转义规则输出字符串的格式化组合子。
;;
;; 语法
;; ----
;; (escaped fmt [quot [esc [rename]]])
;;
;; 参数
;; ----
;; fmt : format?
;; 待转义输出的格式化对象。
;;
;; quot : char?
;; 可选，需要转义的引号字符，默认 `#\"`。
;;
;; esc : char?
;; 可选，转义字符，默认与 quot 相同；为 `#f` 时不转义。
;;
;; rename : (char -> char)?
;; 可选，把无法直接输出的字符映射为替代字符的过程。
;;
;; 返回值
;; ----
;; format?
;; 输出时逐字符转义的格式化组合子。
;;
;; 说明
;; ----
;; 同库的 maybe-escaped 只在字符串满足给定谓词时才转义，
;; 常用于「含空格才加引号」这类展示规则。
;; `(liii format base)` 以 Goldfish 命名重导出这些函数。

(import (liii check) (scheme char) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(define apostrophe (string-ref "x'x" 1))

(check (show #f (escaped "hi, bob!")) => "hi, bob!")
(check (show #f (escaped "hi, 'bob'" apostrophe)) => "hi, \\'bob\\'")
(check (show #f (escaped "hi, 'bob'" apostrophe apostrophe)) => "hi, ''bob''")
(check (show #f (escaped "a\"b")) => "a\\\"b")

(check-report)

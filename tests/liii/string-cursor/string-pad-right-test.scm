(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-pad-right
;; 在字符串右侧补齐字符，使其达到指定长度。
;;
;; 语法
;; ----
;; (string-pad-right s len [char start end])
;;
;; 参数
;; ----
;; s : string
;; 要填充的字符串
;;
;; len : integer
;; 目标长度
;;
;; char : character (可选)
;; 填充字符，默认为 #\space
;;
;; start, end : integer 或 string-cursor? (可选)
;; 子串范围，默认为整个字符串
;;
;; 返回值
;; ------
;; string?
;; 填充后的字符串
;;
;; 说明
;; ----
;; 1. string-pad-right 是 SRFI-130 中的字符串填充函数
;; 2. 与 (liii string) 中的 string-pad-right 功能相同
;; 3. 如果 s 的长度超过 len，则截断右侧
;; 4. 性能：O(len)

(check (string-pad-right "325" 5) => "325  ")
(check (string-pad-right "71325" 5) => "71325")
(check (string-pad-right "8871325" 5) => "88713")
(check (string-pad-right "" 5) => "     ")
(check (string-pad-right "325" 5 #\*) => "325**")

;; 测试中文字符
(check (string-pad-right "中文" 5) => "中文   ")
(check (string-pad-right "中文测试" 3) => "中文测")


;; 测试使用整数索引作为 start/end
(check (string-pad-right "xxabcxx" 5 #\space 2 5) => "abc  ")

;; 测试使用游标作为 start/end
(let* ((s "325")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-pad-right s 5 #\space start end) => "325  "))

;; 测试混合类型报错
(check-catch 'type-error (string-pad-right "abc" 5 #\space 0 (string-cursor-end "abc")))

;; 测试 start > end 报错
(check-catch 'value-error (string-pad-right "abc" 5 #\space 2 1))

;; 测试负数报错
(check-catch 'value-error (string-pad-right "abc" 5 #\space -1 2))

(check-report)

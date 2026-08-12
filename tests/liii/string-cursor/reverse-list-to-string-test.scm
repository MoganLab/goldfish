(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; reverse-list->string
;; 将反转的字符列表转换为字符串。
;;
;; 语法
;; ----
;; (reverse-list->string char-list)
;;
;; 参数
;; ----
;; char-list : list of char?
;; 字符列表（通常是反转顺序）
;;
;; 返回值
;; ------
;; string?
;; 转换后的字符串
;;
;; 说明
;; ----
;; 1. 与 (liii string) 的区别：无直接对应函数，(liii string-cursor) 独有
;; 2. 性能：O(n)，n 为列表长度
;; 3. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确转换
;;
;; 相关实现
;; --------
;; (liii string) 库中无直接对应函数
;; 参见: gf doc liii/string-cursor "reverse-list->string"

(check (reverse-list->string '(#\c #\b #\a)) => "abc")
(check (reverse-list->string '()) => "")

;; Unicode 测试
(check (reverse-list->string (reverse (string->list/cursors "中文"))) => "中文")

;; Emoji 测试
(check (reverse-list->string (reverse (string->list/cursors "hello😀world"))) => "hello😀world")
(check (reverse-list->string (reverse (string->list/cursors "😀🎉🚀"))) => "😀🎉🚀")

(check-report)

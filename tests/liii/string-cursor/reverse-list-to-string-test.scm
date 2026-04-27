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

(check (reverse-list->string '(#\c #\b #\a)) => "abc")
(check (reverse-list->string '()) => "")

;; Unicode 测试
(check (reverse-list->string (reverse (string->list/cursors "中文"))) => "中文")

(check-report)

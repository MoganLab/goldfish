(import (liii check) (liii json schema data))

(check-set-mode! 'report-failed)

;; entries
;; 返回 JSON 对象的键值对序列。
;;
;; 语法
;; ----
;; (entries object)
;;
;; 参数
;; ----
;; object : any?
;; JSON 对象，键值对 alist。
;;
;; 返回值
;; ----
;; list
;; 键值对序列；空对象返回空列表。
;;
;; 注意
;; ----
;; 空对象用 '(()) 表示，entries 会把它规约为空列表，
;; 因此空对象与空列表在遍历时表现一致。
;;
;; 示例
;; ----
;; (entries '(("a" . 1))) => '(("a" . 1))
;; (entries '(())) => '()
;;
;; 错误处理
;; ----
;; 无。

(check (entries '(("a" . 1) ("b" . 2))) => '(("a" . 1) ("b" . 2)))
(check (entries '(())) => '())
(check (entries '()) => '())
(check (entries '("not-an-object")) => '("not-an-object"))

(check-report)

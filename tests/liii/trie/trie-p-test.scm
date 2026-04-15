(import (liii check) (liii trie))


(check-set-mode! 'report-failed)


;; trie?
;; 判断值是否为 trie 数据结构。
;;
;; 语法
;; ----
;; (trie? x)
;;
;; 参数
;; ----
;; x : any?
;; 要检查的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 x 为 trie 时返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (trie? (make-trie)) => #t
;; (trie? 'not-a-trie) => #f


(check-true (trie? (make-trie)))
(check-false (trie? 'not-a-trie))
(check-false (trie? '()))
(check-false (trie? 123))


(check-report)

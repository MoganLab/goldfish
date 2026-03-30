(import (liii check)
        (liii trie)
) ;import

(check-set-mode! 'report-failed)

;; trie-ref*
;; 从 trie 中查询键对应的直接子节点。
;;
;; 语法
;; ----
;; (trie-ref* trie key)
;;
;; 参数
;; ----
;; trie : trie
;; 目标 trie 结构。
;;
;; key : char?
;; 要查询的单字符键。
;;
;; 返回值
;; ----
;; trie | #f
;; 返回键对应的子 trie 节点，如果不存在则返回 #f。
;;
;; 注意
;; ----
;; 这是一个内部辅助函数，用于访问 trie 的直接子节点。
;;
;; 示例
;; ----
;; (trie-ref* trie #\h) => 子 trie 或 #f

(let ((trie (make-trie)))
  (check-false (trie-ref* trie (string->list "hey")))
  (check (trie-ref trie (string->list "hey") 'default) => 'default)
  (check-false (trie-ref* trie (string->list "hey")))
) ;let

(check-report)

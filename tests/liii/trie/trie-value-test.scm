(import (liii check)
        (liii trie)
) ;import

(check-set-mode! 'report-failed)

;; trie-value
;; 获取 trie 节点的值列表。
;;
;; 语法
;; ----
;; (trie-value trie)
;;
;; 参数
;; ----
;; trie : trie
;; 目标 trie 结构。
;;
;; 返回值
;; ----
;; list?
;; 返回该节点存储的值列表，空节点返回空列表。
;;
;; 示例
;; ----
;; (trie-value trie) => '(root-value)
;; (trie-value (make-trie)) => '()

(let ((trie (make-trie)))
  (check (trie-value trie) => '())
) ;let

(let ((trie (make-trie)))
  (trie-insert! trie '() 'root)
  (check (trie-value trie) => '(root))
  (trie-insert! trie (string->list "a") 'letter)
  (check (trie-value trie) => '(root))
) ;let

(let ((trie (make-trie)))
  (trie-insert! trie '() 'root-value)
  (check (trie-value trie) => '(root-value))
  (trie-insert! trie (string->list "test") 'other-value)
  (check (trie-value trie) => '(root-value))
) ;let

(check-report)

(import (liii check) (liii trie))


(check-set-mode! 'report-failed)


;; make-trie
;; 创建一个新的 trie 数据结构。
;;
;; 语法
;; ----
;; (make-trie)
;;
;; 返回值
;; ----
;; trie
;; 返回一个新的空 trie 结构。
;;
;; 示例
;; ----
;; (make-trie) => 新的 trie 结构
;; (trie? (make-trie)) => #t


(check-true (trie? (make-trie)))


(let ((trie (make-trie)))
  (check-true (trie? trie))
  (check (trie-value trie) => '())
  (check (trie->list trie) => '(()))
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie '() 'root)
  (check (trie-ref trie '()) => 'root)
  (check (trie-value trie) => '(root))
) ;let


(check-report)

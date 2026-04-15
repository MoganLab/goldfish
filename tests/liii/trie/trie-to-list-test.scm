(import (liii check)
  (liii sort)
  (liii trie)
) ;import


(check-set-mode! 'report-failed)


;; trie->list
;; 将 trie 转换为列表表示。
;;
;; 语法
;; ----
;; (trie->list trie)
;;
;; 参数
;; ----
;; trie : trie
;; 目标 trie 结构。
;;
;; 返回值
;; ----
;; list?
;; 返回 trie 的列表表示，第一个元素是子节点列表，第二个是节点值。
;;
;; 示例
;; ----
;; (trie->list trie) => '(((#\h ...)) root-value)


(let ((trie (make-trie)))
  (check (trie->list trie) => '(()))
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "hello")
    'world
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "hey")
    'there
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "hi")
    'again
  ) ;trie-insert!

  (check (list-sort! < (trie->list trie))
    =>
    '(((#\h ((#\i () again) (#\e ((#\y () there) (#\l ((#\l ((#\o () world)))))))))))
  ) ;check
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie '() 'root-value)
  (trie-insert! trie
    (string->list "test")
    'other-value
  ) ;trie-insert!
  (check (trie->list trie)
    =>
    '(((#\t ((#\e ((#\s ((#\t () other-value)))))))) root-value)
  ) ;check
) ;let


(check-report)

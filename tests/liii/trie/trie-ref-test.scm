(import (liii check) (liii trie))


(check-set-mode! 'report-failed)


;; trie-ref
;; 从 trie 中查询键对应的值。
;;
;; 语法
;; ----
;; (trie-ref trie key (default #f))
;;
;; 参数
;; ----
;; trie : trie
;; 目标 trie 结构。
;;
;; key : list?
;; 要查询的键，表示为字符列表。
;;
;; default : any?
;; 键不存在时的默认值，默认为 #f。
;;
;; 返回值
;; ----
;; any?
;; 返回键对应的值，如果不存在则返回默认值。
;;
;; 示例
;; ----
;; (trie-ref trie (string->list "hello")) => 'world
;; (trie-ref trie (string->list "nonexistent")) => #f
;; (trie-ref trie (string->list "nonexistent") 'default) => 'default


(let ((trie (make-trie)))
  (check-false (trie-ref trie (string->list "hey"))
  ) ;check-false
  (check (trie-ref trie
           (string->list "hey")
           'default
         ) ;trie-ref
    =>
    'default
  ) ;check
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "hello")
    'world
  ) ;trie-insert!
  (check (trie-ref trie (string->list "hello"))
    =>
    'world
  ) ;check
  (check-false (trie-ref trie (string->list "hell"))
  ) ;check-false
  (check-false (trie-ref trie (string->list "helloo"))
  ) ;check-false
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "app")
    'prefix
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "apple")
    'fruit
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "application")
    'software
  ) ;trie-insert!

  (check (trie-ref trie (string->list "app"))
    =>
    'prefix
  ) ;check
  (check (trie-ref trie (string->list "apple"))
    =>
    'fruit
  ) ;check
  (check (trie-ref trie
           (string->list "application")
         ) ;trie-ref
    =>
    'software
  ) ;check
  (check-false (trie-ref trie (string->list "appl"))
  ) ;check-false
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie '() 'root)
  (trie-insert! trie
    (string->list "a")
    'letter
  ) ;trie-insert!
  (check (trie-ref trie '()) => 'root)
  (check (trie-ref trie (string->list "a"))
    =>
    'letter
  ) ;check
) ;let


(check-report)

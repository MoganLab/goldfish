(import (liii check) (liii trie))


(check-set-mode! 'report-failed)


;; trie-insert!
;; 向 trie 中插入键值对。
;;
;; 语法
;; ----
;; (trie-insert! trie key val)
;;
;; 参数
;; ----
;; trie : trie
;; 目标 trie 结构。
;;
;; key : list?
;; 键，表示为字符列表。
;;
;; val : any?
;; 要插入的值。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 注意
;; ----
;; 如果键已存在，会覆盖原有值。
;;
;; 示例
;; ----
;; (trie-insert! trie (string->list "hello") 'world)
;; (trie-ref trie (string->list "hello")) => 'world


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "hello")
    'world
  ) ;trie-insert!
  (check (trie-ref trie (string->list "hello"))
    =>
    'world
  ) ;check

  ;; 覆盖原有值
  (trie-insert! trie
    (string->list "hello")
    'scheme
  ) ;trie-insert!
  (check (trie-ref trie (string->list "hello"))
    =>
    'scheme
  ) ;check
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "hey")
    'there
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "hi")
    'again
  ) ;trie-insert!
  (check (trie-ref trie (string->list "hey"))
    =>
    'there
  ) ;check
  (check (trie-ref trie (string->list "hi"))
    =>
    'again
  ) ;check
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie
    (string->list "apple")
    'fruit
  ) ;trie-insert!
  (trie-insert! trie
    (string->list "app")
    'prefix
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
) ;let


(let ((trie (make-trie)))
  (trie-insert! trie '() 'root)
  (check (trie-ref trie '()) => 'root)
) ;let


(check-report)

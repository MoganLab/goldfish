(import (liii check) (liii lang))

(define (bitree? x)
  (or (null? x) (bitree :is-type-of x)))

(define-case-class bitree
  ((data string?)
   (left bitree?)
   (right bitree?))
                   
(define (%leaf?)
  (and (null? left) (null? right)))

(typed-define (@leaf (x string?))
  (bitree x '() '()))

(define (%make-string)
  (if (%leaf?)
      ($ "(" :+ data :+ ")")
      ($ (list data (if (null? left) "()" (left :to-string))
                    (if (null? right) "()" (right :to-string)))
          :make-string "(" " " ")")))

(define (%to-string)
  (%make-string))

) ; end of define-case-class






; 测试叶子节点
(check ((bitree :leaf "1") :leaf?) => #t)
(check ((bitree :leaf "1") :to-string) => "(1)")

; 测试非叶子节点
(let ((t (bitree "3" (bitree :leaf "1") (bitree :leaf "2"))))
  (check (t :leaf?) => #f)
  (check (t :to-string) => "(3 (1) (2))"))


; 测试类型检查
(check (bitree? (bitree :leaf "1")) => #t)
(check (bitree? '()) => #t)
(check (bitree? "1") => #f)



; 测试空节点
(check (null? ((bitree :leaf "1") 'left)) => #t)
(check (null? ((bitree :leaf "1") 'right)) => #t)
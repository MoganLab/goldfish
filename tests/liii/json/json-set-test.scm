(import (liii check) (liii json) (liii base) (liii error))

(check-set-mode! 'report-failed)

;; json-set
;; 设置 JSON 路径上的值。
;;
;; 语法
;; ----
;; (json-set json key value)
;; (json-set json key1 key2 ... value)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象或数组。
;;
;; key : symbol? | string? | integer? | boolean?
;; 键名、索引或路径片段。
;;
;; value : any? | procedure?
;; 要写入的新值，或接收旧值并返回新值的函数。
;;
;; 返回值
;; ----
;; any?
;; 返回更新后的新 JSON 数据结构。
;;
;; 注意
;; ----
;; 不会原地修改原对象；对路径中最后一个参数支持函数式更新。
;;
;; 示例
;; ----
;; (json-set '((age . 18)) 'age 19) => '((age . 19))
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象或数组时。

(let* ((j0 '((age . 18) (sex . male)))
       (j1 (json-set j0 'age 19))
       (j2 (json-set j0 'age 'null))
      ) ;
  (check (json-ref j0 'age) => 18)
  (check (json-ref j1 'age) => 19)
  (check (json-ref j2 'age) => 'null)
) ;let*

(let* ((j0 '(("age" . 18) ("sex" . male))) (j1 (json-set j0 "age" 19)))
  (check (json-ref j1 "age") => 19)
  (check (json-ref j0 "age") => 18)
) ;let*

(let* ((j0 #(red green blue)) (j1 (json-set j0 0 'black)))
  (check j0 => #(red green blue))
  (check j1 => #(black green blue))
) ;let*

(let* ((j0 '((bob . 18) (jack . 16)))
       (j1 (json-set j0 #t 3))
       (j2 (json-set j0 #t (lambda (x) (+ x 1))))
      ) ;
  (check j1 => '((bob . 3) (jack . 3)))
  (check j2 => '((bob . 19) (jack . 17)))
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 (json-set j0 'person 'age 26))
      ) ;
  (check (json-ref j1 'person 'age) => 26)
) ;let*

(let* ((j0 '((person (name . "Alice")
               (age . 25)
               (address (city . "Wonderland") (zip . "12345"))))
       ) ;j0
       (j1 (json-set j0 'person 'address 'city "Newland"))
      ) ;
  (check (json-ref j1 'person 'address 'city) => "Newland")
) ;let*

(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-set j0 'age (lambda (x) (+ x 1))))
      ) ;
  (check (json-ref j1 'age) => 26)
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 (json-set j0 'person 'age (lambda (x) (+ x 1))))
      ) ;
  (check (json-ref j1 'person 'age) => 26)
) ;let*

(let* ((j0 '((age . 18) (sex . male))) (j1 20) (j2 (json-set j0 'age j1)))
  (check (json-ref j2 'age) => 20)
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 26)
       (j2 (json-set j0 'person 'age j1))
      ) ;
  (check (json-ref j2 'person 'age) => 26)
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 '((name . "Bob") (age . 30)))
       (j2 (json-set j0 'person j1))
      ) ;
  (check (json-ref j2 'person 'name) => "Bob")
  (check (json-ref j2 'person 'age) => 30)
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 '((address (city . "Wonderland") (zip . "12345"))))
       (j2 (json-set j0 'person j1))
      ) ;
  (check (json-ref j2 'person 'address 'city) => "Wonderland")
  (check (json-ref j2 'person 'address 'zip) => "12345")
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 "Wonderland")
       (j2 (json-set (json-push j0 'person 'city j1) 'person 'age 26))
      ) ;
  (check (json-ref j2 'person 'city) => "Wonderland")
  (check (json-ref j2 'person 'age) => 26)
) ;let*

(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 'null)
       (j2 (json-set j0 'person 'age j1))
      ) ;
  (check (json-ref j2 'person 'age) => 'null)
) ;let*

(check-catch 'type-error (json-set "not-a-json" 'key "val"))
(check-catch 'type-error (json-set 123 'key "val"))

;; 键不存在时返回内容不变的新对象，原对象不受影响
(let* ((j0 '((a . 1))) (j1 (json-set j0 'b 2)))
  (check j1 => '((a . 1)))
  (check j0 => '((a . 1)))
) ;let*

;; 空对象 '(()) 特判：json-set 原样返回，不做任何修改
(check (json-set '(()) 'a 1) => '(()))
(check (json-set '(()) 'a 'b 1) => '(()))

;; 键 #t：对对象/数组的所有值应用更新
(check (json-set '((a . 1) (b . 2)) #t 0) => '((a . 0) (b . 0)))
(check (json-set '((a . 1) (b . 2)) #t (lambda (x) (* x 10)))
  =>
  '((a . 10) (b . 20))
) ;check
(check (json-set #(1 2 3) #t 0) => #(0 0 0))
(check (json-set #(1 2 3) #t (lambda (x) (+ x 1))) => #(2 3 4))

;; 键为过程：按键谓词筛选要更新的条目/元素
(check (json-set '((a . 1) (b . 2)) (lambda (k) (eq? k 'a)) 0)
  =>
  '((a . 0) (b . 2))
) ;check
(check (json-set #(10 20 30) odd? 0) => #(10 0 30))

;; 数组上未匹配的键（越界索引、非整数索引）返回内容不变的新数组
(check (json-set #(1 2) 5 0) => #(1 2))
(check (json-set #(1 2) 'a 0) => #(1 2))

;; 字符串键与 symbol 键互不匹配
(check (json-set '(("age" . 18)) 'age 19) => '(("age" . 18)))
(check (json-set '((age . 18)) "age" 19) => '((age . 18)))

;; 多键路径：中间层不是对象/数组时抛 type-error
(check-catch 'type-error (json-set '((a . 1)) 'a 'b 2))
(check-catch 'type-error (json-set '((a)) 'a 'b 2))

;; 多键路径穿过数组
(let ((j1 (json-set '((a . #(1 2 3))) 'a 1 20)))
  (check (json-ref j1 'a 1) => 20)
) ;let

;; 多键路径 + 叶值为过程
(let ((j1 (json-set '((a (b . 1))) 'a 'b (lambda (x) (+ x 10)))))
  (check (json-ref j1 'a 'b) => 11)
) ;let

;; 多键路径中间层为 '(()) 时原样返回（不深入递归）
(check (json-set '((a (()))) 'a 'b 1) => '((a (()))))

;; 键 #f 的历史怪癖：guenchi 实现落入 (if v ...) 无 else 分支，结果为 #<unspecified>
(check (eq? (json-set '((a . 1)) #f 0) (if #f #f)) => #t)
(check-catch 'wrong-type-arg (json-set #(1 2) #f 0))

(check-report)

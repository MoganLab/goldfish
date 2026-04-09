(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

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

(let* ((j0 `((age . 18) (sex . male)))
       (j1 (json-set j0 'age 19))
       (j2 (json-set j0 'age 'null)))
  (check (json-ref j0 'age) => 18)
  (check (json-ref j1 'age) => 19)
  (check (json-ref j2 'age) => 'null)
) ;let*

(let* ((j0 `(("age" . 18) ("sex" . male)))
       (j1 (json-set j0 "age" 19)))
  (check (json-ref j1 "age") => 19)
  (check (json-ref j0 "age") => 18)
) ;let*

(let* ((j0 #(red green blue))
       (j1 (json-set j0 0 'black)))
  (check j0 => #(red green blue))
  (check j1 => #(black green blue))
) ;let*

(let* ((j0 '((bob . 18) (jack . 16)))
       (j1 (json-set j0 #t 3))
       (j2 (json-set j0 #t (lambda (x) (+ x 1)))))
  (check j1 => '((bob . 3) (jack . 3)))
  (check j2 => '((bob . 19) (jack . 17)))
) ;let*

(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-set j0 'person 'age 26)))
  (check (json-ref j1 'person 'age) => 26)
) ;let*

(let*
  ((j0 '((person . ((name . "Alice")
                    (age . 25)
                    (address . ((city . "Wonderland")
                                (zip . "12345"))))))
                    ) ;address
   (j1 (json-set j0 'person 'address 'city "Newland"))
  ) ;
  (check (json-ref j1 'person 'address 'city) => "Newland")
) ;let*

(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-set j0 'age (lambda (x) (+ x 1)))))
  (check (json-ref j1 'age) => 26)
) ;let*

(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-set j0 'person 'age (lambda (x) (+ x 1)))))
  (check (json-ref j1 'person 'age) => 26)
) ;let*

(let* ((j0 `((age . 18) (sex . male)))
       (j1 20)
       (j2 (json-set j0 'age j1)))
  (check (json-ref j2 'age) => 20)
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 26)
       (j2 (json-set j0 'person 'age j1)))
  (check (json-ref j2 'person 'age) => 26)
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((name . "Bob") (age . 30)))
       (j2 (json-set j0 'person j1)))
  (check (json-ref j2 'person 'name) => "Bob")
  (check (json-ref j2 'person 'age) => 30)
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((address . ((city . "Wonderland") (zip . "12345")))))
       (j2 (json-set j0 'person j1)))
  (check (json-ref j2 'person 'address 'city) => "Wonderland")
  (check (json-ref j2 'person 'address 'zip) => "12345")
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 "Wonderland")
       (j2 (json-set (json-push j0 'person 'city j1) 'person 'age 26)))
  (check (json-ref j2 'person 'city) => "Wonderland")
  (check (json-ref j2 'person 'age) => 26)
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 'null)
       (j2 (json-set j0 'person 'age j1)))
  (check (json-ref j2 'person 'age) => 'null)
) ;let*

(check-catch 'type-error (json-set "not-a-json" 'key "val"))
(check-catch 'type-error (json-set 123 'key "val"))

(check-report)


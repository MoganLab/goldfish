(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-push
;; 在 JSON 中添加新的键值对或数组元素。
;;
;; 语法
;; ----
;; (json-push json key value)
;; (json-push json key1 key2 ... value)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象或数组。
;;
;; key : symbol? | string? | integer? | boolean?
;; 键名、索引或路径片段。
;;
;; value : any?
;; 要插入的新值。
;;
;; 返回值
;; ----
;; any?
;; 返回新增成员后的新 JSON 数据结构。
;;
;; 注意
;; ----
;; 对嵌套对象和嵌套数组都支持逐层进入后追加。
;;
;; 示例
;; ----
;; (json-push '((person . ((name . "Alice")))) 'person 'city "Wonderland") => ...
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象或数组时。

(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-push j0 'person 'city "Wonderland")))
  (check (json-ref j1 'person 'city) => "Wonderland")
) ;let*

(let* ((j0 '(("person" . (("name" . "Alice") ("age" . 25)))))
       (j1 (json-push j0 "person" "city" "Wonderland")))
  (check (json-ref j1 "person" "city") => "Wonderland")
) ;let*

(let*
  ((j0 '((person . ((name . "Alice")
                    (age . 25)
                    (address . ((city . "Oldland")
                                (zip . "12345"))))))
                    ) ;address
   (j1 (json-push j0 'person 'address 'street "Main St"))
  ) ;
  (check (json-ref j1 'person 'address 'street) => "Main St")
) ;let*

(let* ((j0 '((data . #(1 2 3))))
       (j1 (json-push j0 'data 3 4)))
  (check (json-ref j1 'data) => #(1 2 3 4))
) ;let*

(let* ((j0 '((data . #(#(1 2) #(3 4)))))
       (j1 (json-push j0 'data 1 2 5)))
  (check (json-ref j1 'data) => #(#(1 2) #(3 4 5)))
) ;let*

(let* ((j0 '((flags . ((#t . "true") (#f . "false")))))
       (j1 (json-push j0 'flags #t "yes")))
  (check (json-ref j1 'flags #t) => "yes")
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 "Wonderland")
       (j2 (json-push j0 'person 'city j1)))
  (check (json-ref j2 'person 'city) => "Wonderland")
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((city . "Wonderland") (zip . "12345")))
       (j2 (json-push j0 'person 'address j1)))
  (check (json-ref j2 'person 'address 'city) => "Wonderland")
  (check (json-ref j2 'person 'address 'zip) => "12345")
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 'true)
       (j2 (json-push j0 'person 'active j1)))
  (check (json-ref j2 'person 'active) => #t)
) ;let*

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 #(1 2 3))
       (j2 (json-push j0 'person 'scores j1)))
  (check (json-ref j2 'person 'scores) => #(1 2 3))
) ;let*

(check-catch 'type-error (json-push "not-a-json" 'key "val"))
(check-catch 'type-error (json-push 123 'key "val"))

(check-report)


(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import


(check-set-mode! 'report-failed)


;; json-reduce
;; 按键路径或谓词转换 JSON 中的值。
;;
;; 语法
;; ----
;; (json-reduce json key transform-fn)
;; (json-reduce json key1 key2 ... transform-fn)
;; (json-reduce json predicate-fn transform-fn)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象或数组。
;;
;; key : symbol? | string? | integer? | boolean? | procedure?
;; 键路径片段，或用于匹配键的谓词函数。
;;
;; transform-fn : procedure?
;; 接收键和值并返回新值的转换函数。
;;
;; 返回值
;; ----
;; any?
;; 返回变换后的新 JSON 数据结构。
;;
;; 注意
;; ----
;; 对空列表和空向量会安全返回原值；多层路径模式会递归进入嵌套结构。
;;
;; 示例
;; ----
;; (json-reduce '((name . "Alice")) 'name (lambda (k v) (string-upcase v))) => '((name . "ALICE"))
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象、数组或空列表时。


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0
             'name
             (lambda (k v) (string-upcase v))
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((person (name . "Alice") (age . 25)))
       ) ;j0
       (j1 (json-reduce j0
             'person
             (lambda (k v) v)
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'person)
    =>
    '((name . "Alice") (age . 25))
  ) ;check
) ;let*


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0
             (lambda (k) (equal? k 'age))
             (lambda (k v) (+ v 1))
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'age) => 26)
  (check (json-ref j1 'name) => "Alice")
) ;let*


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0
             #t
             (lambda (k v)
               (if (string? v) (string-upcase v) v)
             ) ;lambda
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 #f (lambda (k v) v))
       ) ;j1
      ) ;
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((user (profile (contact (email . "alice@example.com") (phone . "123-456-7890")))))
       ) ;j0
       (j1 (json-reduce j0
             'user
             'profile
             'contact
             'email
             (lambda (k v)
               (string-append v ".verified")
             ) ;lambda
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1
           'user
           'profile
           'contact
           'email
         ) ;json-ref
    =>
    "alice@example.com.verified"
  ) ;check
) ;let*


(let* ((j0 '((user (data (scores . #(85 90 78 92 88)) (settings (notifications . #t) (theme . "dark")))))
       ) ;j0
       (j1 (json-reduce j0
             'user
             'data
             (lambda (k) (equal? k 'scores))
             (lambda (k v)
               (vector-map (lambda (score) (+ score 5))
                 v
               ) ;vector-map
             ) ;lambda
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'user 'data 'scores)
    =>
    #(90 95 83 97 93)
  ) ;check
  (check (json-ref j1
           'user
           'data
           'settings
           'theme
         ) ;json-ref
    =>
    "dark"
  ) ;check
) ;let*


(let* ((j0 '((user (profile (name . "Alice") (age . 25) (scores . #(85 90 78)))))
       ) ;j0
       (j1 (json-reduce j0
             'user
             'profile
             'scores
             (lambda (k v)
               (vector-map (lambda (score) (+ score 5))
                 v
               ) ;vector-map
             ) ;lambda
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'user 'profile 'scores)
    =>
    #(90 95 83)
  ) ;check
  (check (json-ref j1 'user 'profile 'name)
    =>
    "Alice"
  ) ;check
) ;let*


(let ((json '()))
  (check (json-reduce json
           'name
           (lambda (k v) v)
         ) ;json-reduce
    =>
    '()
  ) ;check
) ;let


(let ((json #()))
  (check (json-reduce json
           'name
           (lambda (k v) v)
         ) ;json-reduce
    =>
    #()
  ) ;check
) ;let


(let ((json '((person (name . "Alice") (age . 25) (address (city . "Wonderland") (zip . "12345"))))
      ) ;json
     ) ;
  (let ((updated-json (json-reduce json
                        'person
                        'address
                        'city
                        (lambda (x y) (string-upcase y))
                      ) ;json-reduce
        ) ;updated-json
       ) ;
    (check (json-ref updated-json
             'person
             'address
             'city
           ) ;json-ref
      =>
      "WONDERLAND"
    ) ;check
  ) ;let
) ;let


(check-catch 'type-error
  (json-reduce "not-a-json"
    'key
    (lambda (k v) v)
  ) ;json-reduce
) ;check-catch
(check-catch 'type-error
  (json-reduce 123 'key (lambda (k v) v))
) ;check-catch


(check-report)

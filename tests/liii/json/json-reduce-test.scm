(import (liii check) (liii json) (liii base) (liii error) (scheme char))


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
       (j1 (json-reduce j0 'name (lambda (k v) (string-upcase v))))
      ) ;
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((person (name . "Alice") (age . 25))))
       (j1 (json-reduce j0 'person (lambda (k v) v)))
      ) ;
  (check (json-ref j1 'person) => '((name . "Alice") (age . 25)))
) ;let*


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 (lambda (k) (equal? k 'age)) (lambda (k v) (+ v 1))))
      ) ;
  (check (json-ref j1 'age) => 26)
  (check (json-ref j1 'name) => "Alice")
) ;let*


(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 #t (lambda (k v) (if (string? v) (string-upcase v) v))))
      ) ;
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((name . "Alice") (age . 25))) (j1 (json-reduce j0 #f (lambda (k v) v))))
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'age) => 25)
) ;let*


(let* ((j0 '((user (profile (contact (email . "alice@example.com")
                              (phone . "123-456-7890")))))
       ) ;j0
       (j1 (json-reduce j0
             'user
             'profile
             'contact
             'email
             (lambda (k v) (string-append v ".verified"))
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'user 'profile 'contact 'email)
    =>
    "alice@example.com.verified"
  ) ;check
) ;let*


(let* ((j0 '((user (data (scores . #(85 90 78 92 88))
                     (settings (notifications . #t) (theme . "dark")))))
       ) ;j0
       (j1 (json-reduce j0
             'user
             'data
             (lambda (k) (equal? k 'scores))
             (lambda (k v) (vector-map (lambda (score) (+ score 5)) v))
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'user 'data 'scores) => #(90 95 83 97 93))
  (check (json-ref j1 'user 'data 'settings 'theme) => "dark")
) ;let*


(let* ((j0 '((user (profile (name . "Alice") (age . 25) (scores . #(85 90 78))))))
       (j1 (json-reduce j0
             'user
             'profile
             'scores
             (lambda (k v) (vector-map (lambda (score) (+ score 5)) v))
           ) ;json-reduce
       ) ;j1
      ) ;
  (check (json-ref j1 'user 'profile 'scores) => #(90 95 83))
  (check (json-ref j1 'user 'profile 'name) => "Alice")
) ;let*


(let ((json '()))
  (check (json-reduce json 'name (lambda (k v) v)) => '())
) ;let


(let ((json #()))
  (check (json-reduce json 'name (lambda (k v) v)) => #())
) ;let


(let ((json '((person (name . "Alice")
                (age . 25)
                (address (city . "Wonderland") (zip . "12345"))))
      ) ;json
     ) ;
  (let ((updated-json (json-reduce json 'person 'address 'city (lambda (x y) (string-upcase y)))
        ) ;updated-json
       ) ;
    (check (json-ref updated-json 'person 'address 'city) => "WONDERLAND")
  ) ;let
) ;let


;; 边界用例（0129 json-reduce 迁移 C++ 前锁定语义）

;; 数组按索引转换（p 收到索引和值两个参数）
(let* ((j0 #(10 20 30)) (j1 (json-reduce j0 1 (lambda (k v) (* v 10)))))
  (check j1 => #(10 200 30))
) ;let*

;; 数组谓词键
(let* ((j0 #(10 20 30 40))
       (j1 (json-reduce j0 (lambda (k) (odd? k)) (lambda (k v) (* v 100))))
      ) ;
  (check j1 => #(10 2000 30 4000))
) ;let*

;; 数组 #t 键：全映射，p 收到索引和值两个参数
(let* ((j0 #(1 2 3)) (j1 (json-reduce j0 #t (lambda (k v) (* v 10)))))
  (check j1 => #(10 20 30))
) ;let*

;; 数组 #f 键：历史怪癖，(list->vector x) 抛 wrong-type-arg（与 json-set 的 #f 键一致）
(check-catch 'wrong-type-arg (json-reduce #(1 2 3) #f (lambda (k v) v)))

;; 对象 #f 键原样返回
(let* ((j0 '((a . 1))))
  (check (json-reduce j0 #f (lambda (k v) 99)) => '((a . 1)))
) ;let*

;; 多键路径首键不存在时原样返回
(let* ((j0 '((a . 1))) (j1 (json-reduce j0 'not-exist 'x (lambda (k v) v))))
  (check j1 => '((a . 1)))
) ;let*

;; 空对象 '(()) 原样返回
(let ((j0 '(())))
  (check (json-reduce j0 'a (lambda (k v) v)) => '(()))
) ;let

;; 不可变性：原对象不被修改
(let* ((j0 '((a . 1) (b . 2))) (j1 (json-reduce j0 'a (lambda (k v) (+ v 1)))))
  (check j0 => '((a . 1) (b . 2)))
  (check j1 => '((a . 2) (b . 2)))
) ;let*

;; 缺少转换函数时 value-error
(check-catch 'value-error (json-reduce '((a . 1)) 'a))

(check-catch 'type-error (json-reduce "not-a-json" 'key (lambda (k v) v)))
(check-catch 'type-error (json-reduce 123 'key (lambda (k v) v)))


(check-report)

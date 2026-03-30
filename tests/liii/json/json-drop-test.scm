(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-drop
;; 删除 JSON 中指定路径或满足谓词的元素。
;;
;; 语法
;; ----
;; (json-drop json key)
;; (json-drop json key1 key2 ... target-key)
;; (json-drop json predicate-fn)
;;
;; 参数
;; ----
;; json : any?
;; 目标 JSON 对象或数组。
;;
;; key : symbol? | string? | integer? | boolean? | procedure?
;; 路径片段，或用于筛除当前层级键/索引的谓词函数。
;;
;; 返回值
;; ----
;; any?
;; 返回删除后生成的新 JSON 数据结构。
;;
;; 注意
;; ----
;; 谓词模式只作用于当前层级；路径模式支持深层删除。
;;
;; 示例
;; ----
;; (json-drop json 'address 'city) => ...
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象或数组时。

(let* ((json '((name . "Alice") (age . 25))))
  (let ((updated-json (json-drop json 'age)))
    (check (json-ref updated-json 'age) => '())
  ) ;let
) ;let*

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345")))))
               ) ;address
  (let ((updated-json (json-drop json 'address 'city)))
    (check (json-ref updated-json 'address 'city) => '())
  ) ;let
) ;let*

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345")))))
               ) ;address
  (let ((j1 (json-drop json (lambda (k) (equal? k 'city)))))
    (check (json-ref j1 'address 'city) => "Wonderland")
  ) ;let
  (let ((j2 (json-drop json (lambda (k) (equal? k 'name)))))
    (check (json-ref j2 'name) => '())
  ) ;let
  (let ((j3 (json-drop json 'address (lambda (k) (equal? k 'city)))))
    (check (json-ref j3 'address 'city) => '())
  ) ;let
) ;let*

(let* ((j0 '((name . "Alice") (age . 25) (city . "Wonderland")))
       (j1 (json-drop j0 'age)))
  (check (json-ref j1 'age) => '())
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'city) => "Wonderland")
) ;let*

(let* ((j0 '((user . ((profile . ((name . "Alice")
                                  (age . 25)
                                  (scores . #(85 90 78))))))))
       (j1 (json-drop j0 'user 'profile 'scores)))
  (check (json-ref j1 'user 'profile 'scores) => '())
  (check (json-ref j1 'user 'profile 'name) => "Alice")
  (check (json-ref j1 'user 'profile 'age) => 25)
) ;let*

(let* ((j0 '((data . #(1 2 3 4 5))))
       (j1 (json-drop j0 'data (lambda (k) (and (number? k) (even? k))))))
  (check (json-ref j1 'data) => #(2 4))
) ;let*

(let* ((j0 '((settings . (("theme" . "dark")
                          (notifications . #t)
                          ("language" . "en")))))
       (j1 (json-drop j0 'settings (lambda (k) (string? k)))))
  (check (json-ref j1 'settings "theme") => '())
  (check (json-ref j1 'settings "language") => '())
) ;let*

(let* ((j0 '((a . 1) (b . 2) (c . 3)))
       (j1 (json-drop j0 (lambda (k) (member k '(a c))))))
  (check (json-ref j1 'a) => '())
  (check (json-ref j1 'b) => 2)
  (check (json-ref j1 'c) => '())
) ;let*

(let* ((j0 #())
       (j1 (json-drop j0 0)))
  (check j1 => #())
) ;let*

(check-catch 'type-error (json-drop "not-a-json" 'key))
(check-catch 'type-error (json-drop 123 'key))

(check-report)


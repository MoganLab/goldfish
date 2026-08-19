(import (liii check) (liii json) (liii base) (liii error))

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
               (address (city . "Wonderland") (zip . "12345")))
       ) ;json
      ) ;
  (let ((updated-json (json-drop json 'address 'city)))
    (check (json-ref updated-json 'address 'city) => '())
  ) ;let
) ;let*

(let* ((json '((name . "Alice")
               (age . 25)
               (address (city . "Wonderland") (zip . "12345")))
       ) ;json
      ) ;
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
       (j1 (json-drop j0 'age))
      ) ;
  (check (json-ref j1 'age) => '())
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'city) => "Wonderland")
) ;let*

(let* ((j0 '((user (profile (name . "Alice") (age . 25) (scores . #(85 90 78))))))
       (j1 (json-drop j0 'user 'profile 'scores))
      ) ;
  (check (json-ref j1 'user 'profile 'scores) => '())
  (check (json-ref j1 'user 'profile 'name) => "Alice")
  (check (json-ref j1 'user 'profile 'age) => 25)
) ;let*

(let* ((j0 '((data . #(1 2 3 4 5))))
       (j1 (json-drop j0 'data (lambda (k) (and (number? k) (even? k)))))
      ) ;
  (check (json-ref j1 'data) => #(2 4))
) ;let*

(let* ((j0 '((settings ("theme" . "dark")
               (notifications . #t)
               ("language" . "en"))))
       (j1 (json-drop j0 'settings (lambda (k) (string? k))))
      ) ;
  (check (json-ref j1 'settings "theme") => '())
  (check (json-ref j1 'settings "language") => '())
) ;let*

(let* ((j0 '((a . 1) (b . 2) (c . 3)))
       (j1 (json-drop j0 (lambda (k) (member k '(a c)))))
      ) ;
  (check (json-ref j1 'a) => '())
  (check (json-ref j1 'b) => 2)
  (check (json-ref j1 'c) => '())
) ;let*

(let* ((j0 #()) (j1 (json-drop j0 0)))
  (check j1 => #())
) ;let*

;; 边界用例（0129 json-drop 迁移 C++ 前锁定语义）

;; 全部删空的对象退化为 '()（历史行为）
(let* ((j0 '((a . 1))) (j1 (json-drop j0 'a)))
  (check j1 => '())
) ;let*

;; 空对象 '(()) 单键原样返回
(let ((j0 '(())))
  (check (json-drop j0 'a) => '(()))
) ;let

;; 空对象 '(()) 多键原样返回
(let ((j0 '(())))
  (check (json-drop j0 'a 'b) => '(()))
) ;let

;; 非空数组按索引删除
(let* ((j0 #(10 20 30)) (j1 (json-drop j0 1)))
  (check j1 => #(10 30))
) ;let*

;; 非空数组按索引谓词删除
(let* ((j0 #(10 20 30 40)) (j1 (json-drop j0 (lambda (k) (even? k)))))
  (check j1 => #(20 40))
) ;let*

;; 多键路径中间键不存在时静默不生效
(let* ((j0 '((a . 1) (b . 2))) (j1 (json-drop j0 'not-exist 'x)))
  (check j1 => j0)
) ;let*

;; 多键路径叶层为标量时抛 type-error
(check-catch 'type-error (json-drop '((a . 1)) 'a 'b))

;; 不可变性：原对象不被修改
(let* ((j0 '((a . 1) (b . 2))) (j1 (json-drop j0 'a)))
  (check j0 => '((a . 1) (b . 2)))
  (check j1 => '((b . 2)))
) ;let*

(check-catch 'type-error (json-drop "not-a-json" 'key))
(check-catch 'type-error (json-drop 123 'key))

(check-report)

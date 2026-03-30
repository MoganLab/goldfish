(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-ref
;; 按键路径访问 JSON 对象或数组中的值。
;;
;; 语法
;; ----
;; (json-ref json key)
;; (json-ref json key1 key2 ...)
;;
;; 参数
;; ----
;; json : any?
;; JSON 对象、数组或空列表。
;;
;; key : symbol? | string? | integer? | boolean?
;; 用于访问当前层级值的键或索引。
;;
;; 返回值
;; ----
;; any?
;; 返回键路径对应的值；若路径不存在则返回空列表 `()`。
;;
;; 注意
;; ----
;; 空列表 `()` 会被当作“未找到”透传，以支持安全导航。
;;
;; 示例
;; ----
;; (json-ref bob-j 'bob 'age) => 18
;; (json-ref bob-j 'alice) => '()
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象、数组或空列表时。

(define bob-j
  '((bob . ((age . 18)
            (sex . male)
            (name . "Bob")))) ;define
) ;define

(check (json-ref bob-j 'bob 'age) => 18)
(check (json-ref bob-j 'bob 'sex) => 'male)
(check (json-ref bob-j 'alice) => '())
(check (json-ref bob-j 'alice 'age) => '())
(check (json-ref bob-j 'bob 'name) => "Bob")

(let ((j '((bob . ((age . 18) (sex . male))))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'alice)) => #t)
  (check (json-null? (json-ref j 'bob)) => #f)
) ;let

(let ((j '((alice . ((age . 18) (sex . male))))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'bob)) => #t)
) ;let

(check-catch 'type-error (json-ref "not-a-json" 'key))
(check-catch 'type-error (json-ref 123 'key))

(check-report)


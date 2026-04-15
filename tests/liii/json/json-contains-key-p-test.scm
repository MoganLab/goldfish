(import (liii check)
  (liii json)
  (liii base)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-contains-key?
;; 检查 JSON 对象当前层级是否包含指定键。
;;
;; 语法
;; ----
;; (json-contains-key? json key)
;;
;; 参数
;; ----
;; json : any?
;; 要检查的 JSON 值。
;;
;; key : symbol? | string? | integer? | boolean?
;; 要检查的键名。
;;
;; 返回值
;; ----
;; boolean
;; 键存在时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 仅检查当前层级，不会递归搜索嵌套对象。
;;
;; 示例
;; ----
;; (json-contains-key? j 'bob) => #t
;; (json-contains-key? j 'age) => #f
;;
;; 错误处理
;; ----
;; 无。

(let ((j '((bob (age . 18) (sex . male)))))
  (check-false (json-contains-key? j 'alice)
  ) ;check-false
  (check-true (json-contains-key? j 'bob))
  (check-false (json-contains-key? j 'age)
  ) ;check-false
  (check-false (json-contains-key? j 'sex)
  ) ;check-false
) ;let

(check-false (json-contains-key? (string->json "{}")
               "a"
             ) ;json-contains-key?
) ;check-false
(check-false (json-contains-key? #(1 2 3) 0)
) ;check-false

(check-report)

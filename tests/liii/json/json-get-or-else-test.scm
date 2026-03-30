(import (liii check)
        (liii json)
        (liii base)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; json-get-or-else
;; 在值为 JSON null 时返回默认值。
;;
;; 语法
;; ----
;; (json-get-or-else json default-value)
;;
;; 参数
;; ----
;; json : any?
;; 待检查的 JSON 值。
;;
;; default-value : any?
;; 当 json 为 null 时返回的默认值。
;;
;; 返回值
;; ----
;; any?
;; 非 null 时返回原值；为 null 时返回默认值。
;;
;; 注意
;; ----
;; 仅对 JSON null 生效，不会把空列表 `()` 当作 null。
;;
;; 示例
;; ----
;; (json-get-or-else 'null bob-j) => bob-j
;;
;; 错误处理
;; ----
;; 无。

(define bob-j
  '((bob . ((age . 18)
            (sex . male)
            (name . "Bob")))) ;define
) ;define

(check (json-get-or-else 'null bob-j) => bob-j)
(check (json-get-or-else 42 bob-j) => 42)
(check (json-get-or-else '() bob-j) => '())

(check-report)


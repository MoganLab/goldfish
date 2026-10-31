(import (liii check) (liii json schema) (liii json schema core) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-descend-errors
;; 以指定的根资源校验单个 Schema 节点，返回错误列表。
;;
;; 语法
;; ----
;; (json-schema-descend-errors validator instance schema path schema-path root)
;;
;; 参数
;; ----
;; validator : validator
;; 由 make-json-schema-validator 创建的校验器。
;;
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : any?
;; 待求值的 Schema 节点，可以是对象或布尔值。
;;
;; path : list
;; 当前实例路径，元素按从根到当前位置的顺序排列。
;;
;; schema-path : list
;; 当前 Schema 路径，元素按从根到当前位置的顺序排列。
;;
;; root : any?
;; 根资源，用于解析节点内的相对引用。
;;
;; 返回值
;; ----
;; list
;; 错误对象列表，按发现顺序排列；校验通过时返回空列表。
;;
;; 注意
;; ----
;; 该函数用于在组合关键字内部继续下钻到子 Schema，
;; 并把当前位置的实例路径与 Schema 路径带入错误对象。
;;
;; 示例
;; ----
;; (json-schema-descend-errors validator 1 '(("type" . "integer")) '() '() #t)
;; => '()
;;
;; 错误处理
;; ----
;; json-schema-schema-error 当 Schema 非法时。

(define validator (make-json-schema-validator #t))

;; 合法节点返回空列表
(check (json-schema-descend-errors validator 1 '(("type" . "integer")) '() '() #t)
  =>
  '()
) ;check

;; 非法节点返回错误列表
(let ((errors
        (json-schema-descend-errors validator "x" '(("type" . "integer")) '() '() #t)
      ) ;errors
   ) ;
  (check (length errors) => 1)
  (check (json-schema-error-kind (car errors)) => 'validation)
  (check (json-schema-error-keyword (car errors)) => "type")
  (check (json-schema-error-instance (car errors)) => "x")
) ;let

;; 传入的实例路径与 Schema 路径会体现在错误对象中
(let ((errors
        (json-schema-descend-errors validator "x" '(("type" . "integer"))
          '("rows" 0) '("items") #t
        ) ;json-schema-descend-errors
      ) ;errors
   ) ;
  (check (json-schema-error-path (car errors)) => '("rows" 0))
  (check (json-schema-error-schema-path (car errors)) => '("items" "type"))
  (check (json-schema-error-absolute-path (car errors)) => '("rows" 0))
  (check (json-schema-error-absolute-schema-path (car errors))
    =>
    '("items" "type")
  ) ;check
) ;let

;; 布尔 Schema：false 拒绝一切
(let ((errors (json-schema-descend-errors validator 1 #f '() '() #t))) ;errors
  (check (length errors) => 1)
  (check (json-schema-error-instance (car errors)) => 1)
) ;let

(check-report)

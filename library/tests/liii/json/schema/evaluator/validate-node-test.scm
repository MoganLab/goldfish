;; validate-node
;; 对单个 Schema 节点执行关键字评估，返回错误列表。
;;
;; 语法
;; ----
;; (validate-node validator instance schema path schema-path root)
;;
;; 参数
;; ----
;; validator : validator
;; 由 make-json-schema-validator 创建的校验器。
;;
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : boolean | json-object
;; 当前评估的 Schema 节点。
;;
;; path : list
;; 实例位置指针。
;;
;; schema-path : list
;; Schema 位置指针。
;;
;; root : any?
;; 当前资源根 Schema。
;;
;; 返回值
;; ----
;; list
;; 校验错误列表；空列表表示该节点校验通过。
;;
;; 说明
;; ----
;; true Schema 直接通过；false Schema 总是产生一条 validation 错误；
;; 关键字评估失败后继续收集其余关键字的错误，
;; 除非选项 max-errors 限制了返回数量。

(import (liii check)
  (liii json schema)
  (liii json schema budget)
  (liii json schema evaluator)
) ;import

(check-set-mode! 'report-failed)

(define validator
  (make-json-schema-validator '(("type" . "integer")) '((check-schema? . #f)))
) ;define

;; true Schema 直接通过
(check (validate-node validator 1 #t '() '() #t) => '())

;; false Schema 产生一条错误
(check (length (validate-node validator 1 #f '() '() #f)) => 1)

;; 类型匹配时不产生错误
(check (validate-node validator
         1
         '(("type" . "integer"))
         '()
         '()
         '(("type" . "integer"))
       ) ;validate-node
  =>
  '()
) ;check

;; 类型不匹配时产生一条错误
(check
  (length (validate-node validator
            "abc"
            '(("type" . "integer"))
            '()
            '()
            '(("type" . "integer"))
          ) ;validate-node
  ) ;length
  =>
  1
) ;check

;; max-errors 选项限制返回的错误数量

(define limited-validator
  (make-json-schema-validator '(("type" . "string")
                                ("minLength" . 5)
                                ("maxLength" . 1))
    '((check-schema? . #f) (max-errors . 1))
  ) ;make-json-schema-validator
) ;define

(check
  (length (validate-node limited-validator
            "abc"
            '(("type" . "string") ("minLength" . 5) ("maxLength" . 1))
            '()
            '()
            '(("type" . "string") ("minLength" . 5) ("maxLength" . 1))
          ) ;validate-node
  ) ;length
  =>
  1
) ;check

;; max-errors 命中后不再评估后续关键字（自定义 format checker 不被调用）

(define probe-calls 0)

(define (counting-format value)
  (set! probe-calls (+ probe-calls 1))
  #t
) ;define

(define short-circuit-validator
  (make-json-schema-validator '(("type" . "string")
                                ("format" . "probe")
                                ("minLength" . 10))
    `((formats probe)
      (format-checkers . ,(list (cons 'probe counting-format)))
      (check-schema? . #f)
      (max-errors . 1))
  ) ;make-json-schema-validator
) ;define

(check
  (length (validate-node short-circuit-validator
            1
            '(("type" . "string") ("format" . "probe") ("minLength" . 10))
            '()
            '()
            '(("type" . "string") ("format" . "probe") ("minLength" . 10))
          ) ;validate-node
  ) ;length
  =>
  1
) ;check
(check probe-calls => 0)

;; 携带预算的校验器在评估中推进预算步数计数

(define budget-validator
  (make-json-schema-validator '(("type" . "integer")) '((check-schema? . #f)))
) ;define

(define budget (validator-budget budget-validator))

(reset-resource-budget! budget)
(validate-node budget-validator
  1
  '(("type" . "integer"))
  '()
  '()
  '(("type" . "integer"))
) ;validate-node
(check (> (budget-steps budget) 0) => #t)
(check (budget-depth budget) => 0)

;; Schema 既不是对象也不是布尔值时抛出 json-schema-schema-error
(check-catch 'json-schema-schema-error
  (validate-node validator 1 '("type") '() '() '("type"))
) ;check-catch

(check-report)

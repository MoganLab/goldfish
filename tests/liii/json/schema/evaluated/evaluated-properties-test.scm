;; evaluated-properties
;; 收集对象实例中已被求值的属性名。
;;
;; 语法
;; ----
;; (evaluated-properties validator instance schema root valid? . conditional?)
;;
;; 参数
;; ----
;; validator : any?
;; 当前校验器，用于解析 $ref 与 $dynamicRef。
;;
;; instance : any?
;; 待校验的实例。
;;
;; schema : any?
;; 实例所属的 Schema 对象。
;;
;; root : any?
;; 根资源。
;;
;; valid? : procedure
;; 判断实例是否满足子 Schema 的过程，签名为
;; (valid? instance schema root) => boolean。
;;
;; conditional? : boolean
;; 可选参数，是否递归处理 if/then/else 条件分支，默认为 #t。
;;
;; 返回值
;; ----
;; list
;; 已求值的属性名列表，元素为字符串形式的键。
;;
;; 注意
;; ----
;; 实例不是 JSON 对象或 Schema 不是 JSON 对象时返回空列表。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema evaluated) (liii sort))

(check-set-mode! 'report-failed)

;; valid? 恒为真，表示分支一定匹配。

(define (always-valid? instance schema root)
  #t
) ;define



;; 测试用到的 Schema 片段。

(define type-integer '(("type" . "integer")))

(define property-a (cons "a" type-integer))

(define property-b (cons "b" type-integer))

(define properties-a (list property-a))

(define properties-b (list property-b))

(define schema-properties-a (list (cons "properties" properties-a)))

(define schema-properties-b (list (cons "properties" properties-b)))

(define schema-properties-a-additional
  (list (cons "properties" properties-a) (cons "additionalProperties" #f))
) ;define

(define schema-allOf-properties-b
  (list
    (cons "allOf" (vector (list (cons "properties" properties-b))))
  ) ;list
) ;define

(define schema-anyOf-properties-b
  (list
    (cons "anyOf" (vector (list (cons "properties" properties-b))))
  ) ;list
) ;define

(define schema-oneOf-properties-a
  (list
    (cons "oneOf" (vector (list (cons "properties" properties-a))))
  ) ;list
) ;define

(define schema-dependent-schemas
  (list
    (cons "dependentSchemas"
      (list (cons "a" (list (cons "properties" properties-b))))
    ) ;cons
  ) ;list
) ;define

(define schema-if-then
  (list (cons "if" (list (cons "type" "object")))
    (cons "then" (list (cons "properties" properties-b)))
  ) ;list
) ;define

;; 直接声明的 properties 覆盖对应属性。

(check (evaluated-properties #f
         '(("a" . 1) ("b" . 2))
         schema-properties-a
         #f
         always-valid?
       ) ;evaluated-properties
  =>
  '("a")
) ;check

;; 没有匹配 properties 时不产生已求值属性。

(check (evaluated-properties #f '(("a" . 1)) schema-properties-b #f always-valid?)
  =>
  '()
) ;check

;; additionalProperties 存在时全部属性都算已求值。

(check
  (list-sort string<?
    (evaluated-properties #f
      '(("a" . 1) ("b" . 2))
      schema-properties-a-additional
      #f
      always-valid?
    ) ;evaluated-properties
  ) ;list-sort
  =>
  '("a" "b")
) ;check

;; allOf 分支匹配时合并分支中已求值的属性。

(check (evaluated-properties #f
         '(("a" . 1) ("b" . 2))
         schema-allOf-properties-b
         #f
         always-valid?
       ) ;evaluated-properties
  =>
  '("b")
) ;check

;; anyOf 分支匹配时合并匹配分支中已求值的属性。

(check (evaluated-properties #f
         '(("a" . 1) ("b" . 2))
         schema-anyOf-properties-b
         #f
         always-valid?
       ) ;evaluated-properties
  =>
  '("b")
) ;check

;; oneOf 分支只有一个匹配时才合并。

(check (evaluated-properties #f
         '(("a" . 1))
         schema-oneOf-properties-a
         #f
         always-valid?
       ) ;evaluated-properties
  =>
  '("a")
) ;check

;; dependentSchemas 命中时合并依赖 Schema 中已求值的属性。

(check (evaluated-properties #f
         '(("a" . 1) ("b" . 2))
         schema-dependent-schemas
         #f
         always-valid?
       ) ;evaluated-properties
  =>
  '("b")
) ;check

;; if/then 分支命中时合并 then 分支中已求值的属性。

(check (evaluated-properties #f '(("a" . 1) ("b" . 2)) schema-if-then #f always-valid?)
  =>
  '("b")
) ;check

;; conditional? 为 #f 时不再递归进入 if/then/else。

(check (evaluated-properties #f
         '(("a" . 1) ("b" . 2))
         schema-if-then
         #f
         always-valid?
         #f
       ) ;evaluated-properties
  =>
  '()
) ;check

;; 实例或 Schema 不是对象时返回空列表。

(check (evaluated-properties #f "x" schema-properties-a #f always-valid?)
  =>
  '()
) ;check
(check (evaluated-properties #f '(("a" . 1)) #t #f always-valid?) => '())

(check-report)

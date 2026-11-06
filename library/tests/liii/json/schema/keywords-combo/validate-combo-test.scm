;; validate-combo
;; 评估组合器关键字 allOf、anyOf、oneOf、not 与 if。
;;
;; 语法
;; ----
;; (validate-combo keyword value instance schema path schema-path root
;;   descend valid?)
;;
;; 参数
;; ----
;; keyword : string?
;; 组合器关键字名称。
;;
;; value : any?
;; 关键字取值（子 Schema 或子 Schema 数组）。
;;
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : json-object
;; 当前 Schema 节点。
;;
;; path : list
;; 实例位置指针。
;;
;; 返回值
;; ----
;; list
;; 校验错误列表；空列表表示通过。
;;
;; 说明
;; ----
;; allOf 要求全部子 Schema 通过；anyOf 要求至少一个通过；
;; oneOf 要求恰好一个通过；not 要求子 Schema 不通过；
;; if 按子 Schema 是否通过选择 then 或 else 分支评估，
;; 分支缺失时直接通过。未知关键字返回空列表。

(import (liii check) (liii json schema keywords-combo))

(check-set-mode! 'report-failed)

;; 桩函数：descend 把符号 'PASS 视为通过，其余产生一条错误

(define (stub-descend inst sch p sp r)
  (if (eq? sch 'PASS) '() (list 'ERR))
) ;define

(define (stub-valid? inst sch r)
  (eq? sch 'PASS)
) ;define

(define (run keyword value schema)
  (validate-combo keyword
    value
    'INST
    schema
    '()
    '()
    'ROOT
    stub-descend
    stub-valid?
  ) ;validate-combo
) ;define

;; allOf：全部通过才无错误
(check (run "allOf" '(PASS PASS) '()) => '())
(check (length (run "allOf" '(PASS FAIL) '())) => 1)

;; anyOf：至少一个通过
(check (run "anyOf" '(FAIL PASS) '()) => '())
(check (length (run "anyOf" '(FAIL FAIL) '())) => 1)

;; oneOf：恰好一个通过
(check (run "oneOf" '(PASS FAIL) '()) => '())
(check (length (run "oneOf" '(PASS PASS) '())) => 1)
(check (length (run "oneOf" '(FAIL FAIL) '())) => 1)

;; not：子 Schema 通过时反而报错
(check (length (run "not" 'PASS '())) => 1)
(check (run "not" 'FAIL '()) => '())

;; if：按结果选择 then / else 分支
(check
  (length (run "if" 'PASS '(("then" . FAIL))))
  =>
  1
) ;check
(check (run "if" 'PASS '(("then" . PASS))) => '())
(check (run "if" 'PASS '()) => '())
(check (run "if" 'FAIL '(("else" . PASS))) => '())
(check
  (length (run "if" 'FAIL '(("else" . FAIL))))
  =>
  1
) ;check
(check (run "if" 'FAIL '()) => '())

;; 未知关键字不产生错误
(check (run "maybe" '(PASS) '()) => '())

(check-report)

(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match-let*
;; 顺序匹配并绑定变量，类似于 let*，后续绑定可在其模式或表达式中引用先前的绑定。
;;
;; 语法
;; ----
;; (match-let* ((pattern expr) ...) body ...)
;;
;; 参数
;; ----
;; pattern : pattern
;; 用于解构对应 expr 值的模式。
;;
;; expr : any
;; 待求值并进行模式解构的表达式。
;;
;; body : any
;; 在绑定作用域内执行的主体表达式。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

;; 基础顺序依赖解构
(check
  (match-let*
   ((x 1) ((y) (list (+ x 1))))
   (+ x y)
  ) ;match-let*
  =>
  3
) ;check

;; 模式依赖链测试
(check
  (match-let*
   (((a b) '(10 20)) ((sum) (list (+ a b))) ((doubled) (list (* sum 2))))
   doubled
  ) ;match-let*
  =>
  60
) ;check

;; 空绑定列表
(check (match-let* () 'empty) => 'empty)

;; 匹配失败抛出 match-error
(check-catch 'match-error (match-let* ((x 1) ((y z) '(1))) (+ x y z)))

(check-report)

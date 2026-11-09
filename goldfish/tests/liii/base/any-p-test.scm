;; any?
;; 任意值谓词，恒返回 #t。
;;
;; 语法
;; ----
;; (any? x)
;;
;; 参数
;; ----
;; x : any
;; 待判断的值，任意类型均可。
;;
;; 返回值
;; ----
;; boolean
;; 恒返回 #t。
;;
;; 说明
;; ----
;; any? 与类型标注体系中表示「任意值」的类型 any? 对应，
;; 用于需要接受任意输入的占位场景：它不对参数做任何约束。
;;
;; 需要真正判断「值是否为真」时，请直接使用 (not (eq? x #f))。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii base))


(check-set-mode! 'report-failed)


;; 任意类型都判定为真
(check (any? 1) => #t)
(check (any? "str") => #t)
(check (any? 'sym) => #t)
(check (any? '()) => #t)
(check (any? #t) => #t)
(check (any? #f) => #t)
(check (any? (list 1 2 3)) => #t)


(check-report)

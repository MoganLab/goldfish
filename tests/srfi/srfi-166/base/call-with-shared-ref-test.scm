;; call-with-shared-ref
;; 按共享表为对象输出 `#n=` 定义标签或 `#n#` 引用标签。
;;
;; 语法
;; ----
;; (call-with-shared-ref obj shares each proc)
;;
;; 参数
;; ----
;; obj : any
;; 当前要输出的子对象。
;;
;; shares : pair?
;; extract-shared-objects 返回的共享表。
;;
;; each : format?
;; 用于拼接输出的组合子构造器（通常就是 each）。
;;
;; proc : format?
;; 对象首次出现时紧随标签输出的内容。
;;
;; 返回值
;; ----
;; format?
;; 输出标签与内容的格式化组合子。
;;
;; 说明
;; ----
;; 首次出现输出 `#n=proc`，再次出现只输出 `#n#`；
;; 不在共享表中的对象直接输出 proc。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(define shared (let ((p (list 1))) (list p p)))

(define shares (extract-shared-objects shared #f))

(define part (car shared))

(check (show #f (call-with-shared-ref part shares each (each "X"))) => "#0=X")
(check (show #f (call-with-shared-ref part shares each (each "X"))) => "#0#")
(check (show #f (call-with-shared-ref 42 shares each (each "Z"))) => "Z")

(check-report)

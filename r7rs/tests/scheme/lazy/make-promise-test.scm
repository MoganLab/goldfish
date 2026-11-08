;; make-promise
;; 以急切方式构造 promise：obj 为 promise 时原样返回，否则包装为已求值的 promise。
;;
;; 语法
;; ----
;; (make-promise obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象；若已是 promise 则原样返回。
;;
;; 返回值
;; ----
;; promise?
;; 若 obj 是 promise 则返回 obj 本身；否则返回 force 时得到 obj 的 promise。
;;
;; 说明
;; ----
;; R7RS 4.2.5：(make-promise obj) 类似 delay，但是过程而非语法，
;; 参数不延迟求值。连续包装同一对象两次与一次等价：
;;   (make-promise (make-promise x)) 与 (make-promise x) 行为一致
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (scheme lazy) (scheme base))

(check-set-mode! 'report-failed)

;; 1. 包装普通值
(define mp1 (make-promise 42))
(check (promise? mp1) => #t)
(check (force mp1) => 42)

;; 2. 各类型值
(check (force (make-promise 'a)) => 'a)
(check (force (make-promise "s")) => "s")
(check (force (make-promise '(1 2))) => '(1 2))
(check (force (make-promise #f)) => #f)
(check (force (make-promise #f)) => #f)

;; 3. 已是 promise 时原样返回（同一对象）
(define mp-inner (delay 7))
(define mp-outer (make-promise mp-inner))
(check (eq? mp-outer mp-inner) => #t)

;; 4. 嵌套包装幂等
(check (force (make-promise (make-promise 5))) => 5)

;; 5. 与 delay 结果的互操作
(define mp-d (make-promise (delay 3)))
(check (promise? mp-d) => #t)
(check (force mp-d) => 3)

(check-report)

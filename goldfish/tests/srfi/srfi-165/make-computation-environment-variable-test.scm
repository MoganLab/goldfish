;; make-computation-environment-variable
;; 创建计算环境变量。
;;
;; 语法
;; ----
;; (make-computation-environment-variable name default immutable?)
;;
;; 参数
;; ----
;; name : symbol
;; 变量名，用于报错信息与哈希表键。
;; default : any
;; 变量的默认值。
;; immutable? : boolean
;; 变量是否不可变；为真时不允许就地修改。
;; 返回值
;; ----
;; computation-environment-variable
;; 返回新建的环境变量对象。
;; 说明
;; ----
;; 环境变量是计算环境中的可变槽位，
;; 既可以在创建计算类型时作为静态字段，也可以在运行期通过环境更新绑定。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-165))


(check-set-mode! 'report-failed)


(define v (make-computation-environment-variable 'x 100 #f))

;; 默认值可以被读取
(define env (make-computation-environment))
(check (computation-environment-ref env v) => 100)

;; 每个变量彼此独立
(define w (make-computation-environment-variable 'y 200 #f))
(check (computation-environment-ref env w) => 200)
(check (computation-environment-ref env v) => 100)

(check-report)

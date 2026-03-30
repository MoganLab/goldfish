(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; int-vector
;; 创建只包含整数元素的专用向量。
;;
;; 语法
;; ----
;; (int-vector obj ...)
;;
;; 参数
;; ----
;; obj : integer?
;; 要放入向量的整数元素。
;;
;; 返回值
;; ----
;; vector
;; 一个新的整数向量。
;;
;; 注意
;; ----
;; int-vector 会在构造时检查所有参数是否为整数。
;;
;; 示例
;; ----
;; (int-vector 1 2 3) => 一个包含 1 2 3 的整数向量
;;
;; 错误处理
;; ----
;; wrong-type-arg 当任一参数不是整数时

(check-true (vector? (int-vector 1 2 3)))
(check-catch 'wrong-type-arg (int-vector 1 2 'a))

(check-report)

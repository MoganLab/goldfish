;; expt
;; 计算幂运算。
;;
;;
;; 语法
;; ----
;; (expt z1 z2)
;;
;; 参数
;; ----
;; z1 : number?
;; 底数。
;; z2 : number?
;; 指数。
;;
;; 返回值
;; ----
;; number?
;; z1 的 z2 次幂。
;;
;; 说明
;; ----
;; R7RS 6.2.6：当 z2 为精确整数时，若 z1 为精确数则结果为精确数；
;; 若 z2 为精确非整数且 z1 为精确数，则结果可能为非精确数；
;; 若两者均为非精确数，则结果为非精确数。
;;
;; 1. 精确整数指数：保持精确性
;;    (expt 2 10) => 1024
;;    (expt 2 -1) => 1/2
;;    (expt 0 0) => 1
;; 2. 非精确操作数：结果为非精确数
;;    (expt 2.0 3) => 8.0
;; 3. 精确底数与非精确指数混合
;;    (expt 4 1/2) => 2（实现定义，本实现返回精确数）
;;
;; 错误处理
;; ----
;; division-by-zero
;; 当 z1 为 0 且 z2 为负数时抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 整数指数
(check (expt 2 3) => 8)
(check (expt 2 0) => 1)
(check (expt 2 -1) => 1/2)
(check (expt 0 0) => 1)
(check (expt 2 10) => 1024)
(check (expt -2 3) => -8)
(check (expt -2 4) => 16)
(check (expt 1 100) => 1)
(check (expt 10 -2) => 1/100)

;; 有理数指数
(check (expt 4 1/2) => 2)
(check (expt 1/4 1/2) => 1/2)
(check (expt 1/2 -2) => 4)

;; 浮点数与非精确数
(check (expt 2.0 3) => 8.0)
(check (expt 2.0 -1) => 0.5)
(check (expt 0.0 0) => 0.0)

;; 错误场景
(check-catch 'division-by-zero (expt 0 -1))

(check-report)

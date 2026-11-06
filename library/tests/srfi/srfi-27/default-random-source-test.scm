;; default-random-source
;; 默认随机源对象。
;;
;; 语法
;; ----
;; default-random-source
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; random-source
;; 默认随机源对象；random-integer / random-real 使用的随机源。
;;
;; 说明
;; ----
;; 1. 该绑定是随机源对象本身，不是过程。
;; 2. 对其执行 random-source-randomize! 可用当前时间重新播种。

(import (srfi srfi-27) (liii check))

(check-set-mode! 'report-failed)

(check-true (random-source? default-random-source))

;; 多次取用都是同一个默认随机源
(check (eq? default-random-source default-random-source) => #t)

(check-report)

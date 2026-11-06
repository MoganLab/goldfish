;; set-goldtool-jobs!
;; 设置工具链文件级并发的 worker 数量。
;; 仅接受不小于 1 的数，其余取值回退为 1（串行执行）。
;;
;; 语法
;; ----
;; (set-goldtool-jobs! n)
;;
;; 参数
;; ----
;; n : number?
;; worker 数量，小于 1 或非数值时按 1 处理。
;;
;; 返回值
;; ------
;; unspecified
;; 并发度设置完成后返回。

(import (scheme base) (liii check) (liii goldtool parallel))

(check-set-mode! 'report-failed)

(define original-jobs (goldtool-jobs))

;; 合法的并发度被记录
(set-goldtool-jobs! 4)
(check (goldtool-jobs) => 4)

;; 小于 1 的并发度回退为 1
(set-goldtool-jobs! 0)
(check (goldtool-jobs) => 1)
(set-goldtool-jobs! -3)
(check (goldtool-jobs) => 1)

;; 非数值回退为 1
(set-goldtool-jobs! #f)
(check (goldtool-jobs) => 1)

;; 恢复原值，避免影响其它测试
(set-goldtool-jobs! original-jobs)
(check (goldtool-jobs) => original-jobs)

(check-report)

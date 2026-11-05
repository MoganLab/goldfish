;; main
;; 启动 Goldfish Scheme 交互式 REPL，需要真实的终端环境。
;;
;; 语法
;; ----
;; (main)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受参数。
;;
;; 返回值
;; ----
;; integer
;; REPL 正常退出时返回 0。

(import (scheme base) (liii check) (liii goldrepl))

(check-set-mode! 'report-failed)

;; REPL 入口是可调用的过程
;; 实际启动需要交互式终端，单元测试只验证入口函数的类型
(check (procedure? main) => #t)

;; REPL 依赖的各个子模块功能均已导出
(check (procedure? %repl-mode) => #t)
(check (procedure? %repl-show-out) => #t)
(check (procedure? %repl-refresh!) => #t)

(check-report)

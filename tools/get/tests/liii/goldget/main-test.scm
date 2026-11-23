;; main
;; gf get 命令入口：解析参数并分发到帮助/错误/安装。
;;
;; 语法
;; ----
;; (main)
;;
;; 参数
;; ----
;; 无（读取 (command-line)）。
;;
;; 返回值
;; ----
;; integer
;; 退出码；帮助为 0，参数错误为 1。

(import (liii check) (liii goldget))

(check-set-mode! 'report-failed)

;; main 可调用；真实安装链路由 get-install 的单测覆盖，
;; 此处不触碰用户 ~/.gf。
(check-true (procedure? main))

(check-report)

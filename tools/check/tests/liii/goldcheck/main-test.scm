;; main
;; check 命令行工具的主入口过程。
;;
;; 语法
;; ----
;; (main)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; 不返回
;; 解析命令行参数并以相应退出状态码退出程序。

(import (scheme base) (liii check) (liii goldcheck))

(check-set-mode! 'report-failed)

;; 1. 验证 main 过程存在且为过程对象
(check-true (procedure? main))

(check-report)

;; main
;; Pi CLI 命令行工具的主入口过程。
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
;; ----
;; 不返回
;; 读取命令行参数并以相应退出状态码退出程序。

;; 先导入 (liii pi code cli) 避免宏展开期常量回收
(import (liii pi code cli) (liii check))

(check-set-mode! 'report-failed)

;; 验证 main 过程存在且为无参过程
(check-true (procedure? main))


(check-report)

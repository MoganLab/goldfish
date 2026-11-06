;; executable
;; 返回当前运行程序的可执行文件路径。
;;
;; 语法
;; ----
;; (executable)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; 当前程序可执行文件的路径。
;;
;; 说明
;; ----
;; 常用于定位同目录下的资源文件或辅助脚本。

(import (liii check)
        (liii sys))

(check-set-mode! 'report-failed)

(check-true (string? (executable)))
(check-true (> (string-length (executable)) 0))

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (executable 1))

(check-report)

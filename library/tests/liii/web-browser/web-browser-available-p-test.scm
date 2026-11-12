;; web-browser-available?
;; 检查当前系统和环境下是否有可用的 Web 浏览器启动器。
;;
;; 语法
;; ----
;; (web-browser-available?)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; boolean?
;; 如果环境中检测到可用浏览器启动器返回 #t，否则返回 #f。

(import (liii check)
        (liii os)
        (liii web-browser))

(check-set-mode! 'report-failed)

;; 基本返回值类型测试
(check-true (boolean? (web-browser-available?)))

;; 环境变量覆盖测试
(let ((old-browser (getenv "BROWSER")))
  ;; 存在可用命令时应返回 #t
  (putenv "BROWSER" "sh")
  (check-true (web-browser-available?))

  ;; 恢复环境变量
  (if old-browser
      (putenv "BROWSER" old-browser)
      (unsetenv "BROWSER")))

(check-report)

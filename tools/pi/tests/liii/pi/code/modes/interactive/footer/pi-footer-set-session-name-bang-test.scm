;; pi-footer-set-session-name!
;; 执行 pi-footer-set-session-name! 操作并更新状态。
;;
;; 语法
;; ----
;; (pi-footer-set-session-name! ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

;; 会话名称显示与动态更新，不影响分支和模型状态
(let* ((ft (make-pi-footer :cwd "/tmp/named" :branch "main"
             :provider "google" :model "gemini-3.8-flash"
             :session-name "Refactor auth module"))
       (line1 (list-ref (component-render ft 100) 0)))
  (check-true (string-contains? line1 "/tmp/named (main) • Refactor auth module"))
  (pi-footer-set-session-name! ft "Fix Windows build")
  (let ((updated (list-ref (component-render ft 100) 0)))
    (check-true (string-contains? updated "/tmp/named (main) • Fix Windows build"))
    (check-false (string-contains? updated "Refactor auth module")))
  (pi-footer-set-session-name! ft #f)
  (check-true (string-contains? (list-ref (component-render ft 100) 0) "/tmp/named (main)")))

(check-report)

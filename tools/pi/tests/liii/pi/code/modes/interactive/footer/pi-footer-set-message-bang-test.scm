;; pi-footer-set-message!
;; 设置页脚临时提示消息并触发组件重绘。
;;
;; 语法
;; ----
;; (pi-footer-set-message! comp msg)
;;
;; 参数
;; ----
;; comp : pi-footer?
;; 页脚组件对象。
;; msg : (or string? #f)
;; 要显示的提示消息，或 #f 清空。
;;
;; 返回值
;; -----
;; unspecified

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

(let ((ft (make-pi-footer :cwd "/tmp" :provider "google" :model "gemini")))
  (check (pi-footer-message ft) => #f)
  (pi-footer-set-message! ft "Copied to clipboard")
  (check (pi-footer-message ft) => "Copied to clipboard")
  (pi-footer-set-message! ft #f)
  (check (pi-footer-message ft) => #f)
) ;let

(check-report)

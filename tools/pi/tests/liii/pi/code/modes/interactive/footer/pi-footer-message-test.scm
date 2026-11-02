;; pi-footer-message
;; 获取页脚当前设置的提示消息（若无则返回 #f）。
;;
;; 语法
;; ----
;; (pi-footer-message comp)
;;
;; 参数
;; ----
;; comp : pi-footer?
;; 页脚组件对象。
;;
;; 返回值
;; -----
;; (or string? #f)
;; 当前提示消息文本或 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive footer)
) ;import

(check-set-mode! 'report-failed)

(let ((ft (make-pi-footer :cwd "/tmp" :provider "google" :model "gemini")))
  (check (pi-footer-message ft) => #f)
  (pi-footer-set-message! ft "Ready")
  (check (pi-footer-message ft) => "Ready")
) ;let

(check-report)

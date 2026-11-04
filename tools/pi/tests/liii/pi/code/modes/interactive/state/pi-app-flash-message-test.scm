;; pi-app-flash-message
;; 读取应用当前未过期的瞬态浮层提示（Flash）文本。
;;
;; 语法
;; ----
;; (pi-app-flash-message app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; string? 或 #f
;; flash 未过期时返回其消息文本，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii time)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 未设置 flash 时返回 #f
  (check-false (pi-app-flash-message app))

  ;; 设置后未过期时返回消息文本
  (pi-app-flash! app "Copied!" 10000)
  (check (pi-app-flash-message app) => "Copied!")

  ;; 设置极短 flash 并等待过期后返回 #f
  (pi-app-flash! app "Gone!" 20)
  (sleep 0.1)
  (check-false (pi-app-flash-message app))

  ;; 过期后重新设置仍可读取
  (pi-app-flash! app "Again!" 10000)
  (check (pi-app-flash-message app) => "Again!")
) ;let*

(check-report)

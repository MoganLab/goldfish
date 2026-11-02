;; pi-app-selection
;; 获取应用当前存储的选区范围对象（若无则为 #f）。
;;
;; 语法
;; ----
;; (pi-app-selection app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; any
;; 当前字段存储的选区状态值。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-selection app) => #f)
  (let ((sel (make-selection-range '(0 . 0) '(5 . 5) #t)))
    (%pi-app-selection-set! app sel)
    (check (pi-app-selection app) => sel)
  ) ;let
) ;let

(check-report)

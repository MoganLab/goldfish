;; %pi-app-selection-set!
;; 底层设置应用状态字段 selection。
;;
;; 语法
;; ----
;; (%pi-app-selection-set! app val)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; val : any
;; 选区范围对象或 #f。
;;
;; 返回值
;; -----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (sel (make-selection-range '(1 . 1) '(4 . 4) #t))
      ) ;
  (%pi-app-selection-set! app sel)
  (check (pi-app-selection app) => sel)
  (%pi-app-selection-set! app #f)
  (check (pi-app-selection app) => #f)
) ;let*

(check-report)

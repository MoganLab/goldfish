;; pi-app-copy-selection!
;; 将应用当前激活的鼠标选区文本复制到系统剪贴板（若无激活选区则返回 #f）。
;;
;; 语法
;; ----
;; (pi-app-copy-selection! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; boolean?
;; 成功复制返回 #t，无选区或选区文本为空返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-mouse)
  (liii pi code modes interactive state)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 无选区时返回 #f
  (check-false (pi-app-copy-selection! app))

  ;; 填充内容并激活选区
  (pi-app-add-user-message! app "line 0 of chat content")
  (pi-app-add-user-message! app "line 1 of chat content")
  (pi-app-render! app)
  (pi-app-selection-set! app (make-selection-range '(4 . 4) '(15 . 4) #t))
  (check-true (pi-app-copy-selection! app))
) ;let*

(check-report)

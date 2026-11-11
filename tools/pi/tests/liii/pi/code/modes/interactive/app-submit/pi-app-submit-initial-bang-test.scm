;; pi-app-submit-initial!
;; 向应用提交启动时传入的初始消息与后续补充提示列表。
;;
;; 语法
;; ----
;; (pi-app-submit-initial! app initial-msg rest-msgs)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; initial-msg : string
;; 命令行传入的首个 prompt 文本。
;; rest-msgs : list of string
;; 命令行传入的其余 prompt 文本列表。
;;
;; 返回值
;; ------
;; any
;; 执行消息依次提交的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 依次分发初始 prompt 列表
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (received '())
       (app
         (make-pi-app
           :terminal vterm
           :handler  (lambda (a msg) (set! received (cons msg received)))
         ) ;make-pi-app
       ) ;app
      ) ;
  (pi-app-submit-initial! app "first" '("second"))
  (check (reverse received) => '("first" "second"))
) ;let*

(check-report)

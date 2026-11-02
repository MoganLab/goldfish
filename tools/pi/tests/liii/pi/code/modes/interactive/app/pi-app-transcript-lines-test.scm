;; pi-app-transcript-lines
;; 导出交互式应用当前的完整历史对话卡片渲染文本行列表。
;;
;; 语法
;; ----
;; (pi-app-transcript-lines app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; list of string
;; 会话历史中全部消息卡片按终端宽度渲染后的纯文本行列表。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始空历史返回空列表
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (lines (pi-app-transcript-lines app))
      ) ;
  (check lines => '())
) ;let*

;; 2. 存在消息卡片时返回渲染行列表
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-add-user-message! app "hello transcript")
  (let ((lines (pi-app-transcript-lines app)))
    (check-true (pair? lines))
    (check-true (any (lambda (line) (string-contains? line "hello transcript")) lines))
  ) ;let
) ;let*

(check-report)

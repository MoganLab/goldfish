;; pi-app-add-tool-result!
;; 向应用对话记录追加工具调用执行结果卡片。
;;
;; 语法
;; ----
;; (pi-app-add-tool-result! app tool-name args status . maybe-result)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; tool-name : string
;; 工具名称。
;; args : any
;; 工具调用参数。
;; status : symbol
;; 执行状态。
;; maybe-result : any
;; 可选执行结果。
;;
;; 返回值
;; ------
;; any
;; 执行追加卡片的副作用。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 追加工具卡片后聊天容器卡片数增加
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (length (pi-app-chat app)) => 0)
  (pi-app-add-tool-result! app "bash" '() 'success "output text")
  (check (length (pi-app-chat app)) => 1)
) ;let

(check-report)

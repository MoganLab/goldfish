;; handle-verbose-result
;; 处理 verbose 运行模式的最终响应状态并返回退出码。
;;
;; 语法
;; ----
;; (handle-verbose-result last-msg [port])
;;
;; 参数
;; ----
;; last-msg : any
;; 最后的响应消息，可为 pi-message 记录、alist 或 #f。
;;
;; port : output-port?
;; 输出端口（可选）。
;;
;; 返回值
;; ----
;; integer?
;; 成功返回 0，错误返回 1。

(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code modes verbose)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常 assistant 消息返回 0
(let ((msg (pi-assistant-message (list (pi-text-block "Done")) :stop-reason "stop")))
  (check (handle-verbose-result msg) => 0)
) ;let

;; 2. 发生错误时返回 1
(let ((msg (list (cons 'role "assistant")
                 (cons 'stopReason "error")
                 (cons 'errorMessage "Something broke")
           ) ;list
     )) ;msg
  (check (handle-verbose-result msg) => 1)
) ;let

;; 3. 达到长度截断时返回 1
(let ((msg (list (cons 'role "assistant")
                 (cons 'stopReason "length")
           ) ;list
     )) ;msg
  (check (handle-verbose-result msg) => 1)
) ;let

;; 4. 空消息警告返回 0
(check (handle-verbose-result #f) => 0)

(check-report)

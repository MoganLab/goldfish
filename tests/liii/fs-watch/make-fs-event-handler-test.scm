(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

;; make-fs-event-handler
;; 创建文件系统事件处理器（对齐 Python watchdog 的 FileSystemEventHandler），
;; 持有四个事件回调槽。
;;
;; 语法
;; ----
;; (make-fs-event-handler [:on-modified cb] [:on-created cb]
;;                        [:on-deleted cb] [:on-moved cb]) → fs-event-handler?
;;
;; 参数
;; ----
;; :on-modified : procedure | #f (可选，默认 #f)
;; "modified" 事件回调，签名 (lambda (ctx event) ...)。
;; :on-created : procedure | #f (可选，默认 #f)
;; "created" 事件回调。
;; :on-deleted : procedure | #f (可选，默认 #f)
;; "deleted" 事件回调。
;; :on-moved : procedure | #f (可选，默认 #f)
;; "moved" 事件回调（事件带 dest-path 目标路径）。
;;
;; 返回值
;; -----
;; fs-event-handler?
;; 事件处理器对象，供 observer-schedule! 注册。
;;
;; 说明
;; ----
;; 1. 回调在后台 (liii go) worker 会话中执行，不在主会话（对齐 watchdog
;;    回调运行于 observer 线程的语义）。
;; 2. 回调签名为 (ctx event)：ctx 为 observer-schedule! 的 :context 传入值
;;    （跨线程状态传递通道，典型为 channel，等价 Python 回调持有 queue.Queue），
;;    event 为 <fs-event> 记录。
;; 3. 回调体内引用的自由符号必须是库导出绑定（如 chan-send!、fs-event-path），
;;    不能闭包捕获主会话对象（过程经源码序列化在 worker 会话重建）。
;;
;; 错误处理
;; ----
;; 任一回调槽传入非 procedure 且非 #f 时抛出 type-error。

;; 四个回调槽的存取与缺省 #f
(let ((h (make-fs-event-handler
           :on-modified (lambda (ctx ev) 'mod)
           :on-created  (lambda (ctx ev) 'cre)
         ) ;make-fs-event-handler
      ) ;h
     ) ;
  (check-true (fs-event-handler? h))
  (check-true (procedure? (fs-handler-on-modified h)))
  (check-true (procedure? (fs-handler-on-created h)))
  (check-false (fs-handler-on-deleted h))
  (check-false (fs-handler-on-moved h))
) ;let

;; 回调槽类型校验
(check-catch 'type-error (make-fs-event-handler :on-modified "not-proc"))

(check-report)

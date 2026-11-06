;; fs-observer-schedule!
;; 注册监听项（对齐 watchdog 的 Observer.schedule）。
;;
;; 语法
;; ----
;; (fs-observer-schedule! obs handler path)
;; (fs-observer-schedule! obs handler path :recursive #f :context #f)
;;
;; 参数
;; ----
;; obs : fs-observer?
;; observer 对象。
;; handler : fs-event-handler?
;; 事件处理器（回调在 go worker 会话执行，见 make-fs-event-handler 文档）。
;; path : string?
;; 要监听的目录。
;; :recursive : boolean? (可选，默认 #f)
;; 递归监听暂不支持，只接受 #f。
;; :context : any (可选，默认 #f)
;; 传给回调的 ctx 值；须为可跨 go 会话序列化的值（channel/字符串/数字等）。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; start 前可多次累积注册；running 中注册抛 value-error（v1 要求
;; start 前完成注册，与 watchdog 的 schedule 语义差异）。
;;
;; 错误处理
;; ----
;; obs 非 observer、handler 非 fs-event-handler、path 非字符串时抛 type-error；
;; observer 运行中注册时抛 value-error。

(import (scheme base) (liii check) (liii tempfile) (liii fs-watch) (liii shutil))

(check-set-mode! 'report-failed)

;; 注册后允许继续累积注册（start 前）
(let* ((h (make-fs-event-handler))
       (obs (make-fs-observer))
       (dir (make-temp-dir "gf-sched-"))
      ) ;
  (fs-observer-schedule! obs h dir)
  (fs-observer-schedule! obs h dir :recursive #f :context 'ctx)
  (rmtree dir #t)
) ;let*

;; 参数校验
(let ((obs (make-fs-observer))
      (h (make-fs-event-handler))
      (dir (make-temp-dir "gf-sched-"))
     ) ;
  (check-catch 'type-error (fs-observer-schedule! "x" h dir))
  (check-catch 'type-error (fs-observer-schedule! obs "x" dir))
  (check-catch 'type-error (fs-observer-schedule! obs h 123))
  (rmtree dir #t)
) ;let

(check-report)

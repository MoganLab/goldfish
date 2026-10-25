(import (scheme base) (liii check) (liii tempfile) (liii fs-watch))

(check-set-mode! 'report-failed)

;; make-observer
;; 创建文件系统监听 observer（对齐 Python watchdog 的 Observer）。
;;
;; 语法
;; ----
;; (make-observer) → observer?
;; (observer? obj) → boolean?
;;
;; 返回值
;; -----
;; observer?
;; 新的 observer 对象；初始无监听项、未运行。
;;
;; 说明
;; ----
;; 生命周期：make-observer → observer-schedule!（可多次）→ observer-start!
;; → ... → observer-stop!（幂等）→ 可再次 schedule + start 复用。

(check-true (observer? (make-observer)))
(check-false (observer? "not-observer"))

;; observer-stop!
;; 停止监听：逐个停掉 C 层 watcher（后台泵线程收到 'stopped 后自行退出）。
;;
;; 语法
;; ----
;; (observer-stop! obs) → unspecified
;;
;; 说明
;; ----
;; 1. 幂等，可重复调用。
;; 2. 未 start 直接 stop 无害。
;; 3. stop 后允许重新 schedule + start（复用同一 observer）。
;;
;; 错误处理
;; ----
;; obs 非 observer 时抛出 type-error。

;; observer-schedule!
;; 注册监听项（对齐 watchdog 的 Observer.schedule）。
;;
;; 语法
;; ----
;; (observer-schedule! obs handler path [:recursive #f] [:context #f]) → unspecified
;;
;; 参数
;; ----
;; obs : observer?
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
;; 说明
;; ----
;; start 前可多次累积注册；running 中注册抛 value-error（v1 要求
;; start 前完成注册，与 watchdog 的 schedule 语义差异）。
;;
;; 错误处理
;; ----
;; obs 非 observer、handler 非 fs-event-handler、path 非字符串时抛 type-error；
;; observer 运行中注册时抛 value-error。

;; observer-start!
;; 启动监听：为每个已注册监听项创建 C 层 watcher 并派发后台 go 泵线程。
;;
;; 语法
;; ----
;; (observer-start! obs) → unspecified
;;
;; 说明
;; ----
;; 1. 回调源码列表与 :context 均为可序列化值，安全跨越 go 会话边界。
;; 2. 后台泵（fs-watch-pump）在 (liii go) worker 会话循环 poll 事件并
;;    分发到对应回调；observer-stop! 后自动退出。
;; 3. 重复 start 抛 value-error。

(let* ((h (make-fs-event-handler :on-modified (lambda (ctx ev) #t)))
       (obs (make-observer))
       (dir (make-temp-dir "gf-sched-"))
      ) ;
  ;; 未 start 直接 stop：无害
  (observer-stop! obs)
  (observer-schedule! obs h dir)
  (observer-start! obs)
  ;; running 中再 schedule → value-error
  (check-catch 'value-error (observer-schedule! obs h dir))
  ;; 重复 start → value-error
  (check-catch 'value-error (observer-start! obs))
  (observer-stop! obs)
  ;; stop 后可重新 schedule + start
  (observer-schedule! obs h dir)
  (observer-start! obs)
  (observer-stop! obs)
  ;; 重复 stop 幂等
  (observer-stop! obs)
  (g_rmtree dir)
) ;let*

;; 参数校验
(let ((obs (make-observer))
      (h (make-fs-event-handler))
      (dir (make-temp-dir "gf-sched-"))
     ) ;
  (check-catch 'type-error (observer-schedule! "x" h dir))
  (check-catch 'type-error (observer-schedule! obs "x" dir))
  (check-catch 'type-error (observer-schedule! obs h 123))
  (check-catch 'type-error (observer-stop! "x"))
  (check-catch 'type-error (observer-start! "x"))
  (g_rmtree dir)
) ;let

(check-report)

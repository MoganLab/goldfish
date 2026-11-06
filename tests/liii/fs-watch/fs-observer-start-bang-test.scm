;; fs-observer-start!
;; 启动监听：为每个已注册监听项创建 C 层 watcher 并派发后台 go 泵线程。
;;
;; 语法
;; ----
;; (fs-observer-start! obs)
;;
;; 参数
;; ----
;; obs : fs-observer?
;; observer 对象。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 回调源码列表与 :context 均为可序列化值，安全跨越 go 会话边界。
;; 2. 后台泵（fs-watch-pump）在 (liii go) worker 会话循环 poll 事件并
;;    分发到对应回调；fs-observer-stop! 后自动退出。
;; 3. 重复 start 抛 value-error。
;;
;; 错误处理
;; ----
;; obs 非 observer 时抛 type-error；observer 运行中重复 start 抛 value-error。

(import (scheme base) (liii check) (liii tempfile) (liii fs-watch) (liii shutil))

(check-set-mode! 'report-failed)

;; start / stop 生命周期：start 后 running 中再 schedule 抛 value-error，
;; 重复 start 抛 value-error，stop 幂等
(let* ((h (make-fs-event-handler :on-modified (lambda (ctx ev) #t)))
       (obs (make-fs-observer))
       (dir (make-temp-dir "gf-sched-"))
      ) ;
  (fs-observer-schedule! obs h dir)
  (fs-observer-start! obs)
  (check-catch 'value-error (fs-observer-schedule! obs h dir))
  (check-catch 'value-error (fs-observer-start! obs))
  (fs-observer-stop! obs)
  ;; stop 后可重新 schedule + start
  (fs-observer-schedule! obs h dir)
  (fs-observer-start! obs)
  (fs-observer-stop! obs)
  (rmtree dir #t)
) ;let*

;; 参数校验
(check-catch 'type-error (fs-observer-start! "x"))

(check-report)

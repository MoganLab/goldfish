(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; go
;; 将一段代码调度到后台 worker 线程并发执行（每个 worker 独占一个 s7 会话）。
;;
;; 语法
;; ----
;; (go (captured-var ...) body ...)
;; (go body ...)
;;
;; 参数
;; ----
;; captured-var : symbol
;; 需要从当前环境捕获传入 worker 的变量名。
;;
;; body : any
;; 在 worker 会话中执行的代码。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. worker 间通过 channel 传递数据与结果（深拷贝，彻底隔离 GC 堆）。
;; 2. 捕获变量只支持可序列化的数据类型，不支持过程/闭包（spawn 时抛 type-error）。
;; 3. body 中可直接引用全局函数名（如 car、display），无需捕获。
;; 4. worker 内的运行时异常不会使线程池崩溃，错误输出到 stderr。

;; 1. 基本 go 任务启动与通道通信
(define ch1 (make-chan 1))
(go (ch1)
  (chan-send! ch1 "hello from worker" 5000))

(define msg1 (chan-recv! ch1 5000))
(check msg1 => "hello from worker")

;; 2. 多个 worker 并发向同一个 channel 写入计算结果
(define results (make-chan 10))
(define (spawn-worker i)
  (go (results i)
    (chan-send! results (* i i) 5000)))

;; 启动 5 个并发任务
(spawn-worker 1)
(spawn-worker 2)
(spawn-worker 3)
(spawn-worker 4)
(spawn-worker 5)

;; 收集所有结果
(define collected '())
(let loop ((count 0))
  (if (< count 5)
      (let ((val (chan-recv! results 5000)))
        (set! collected (cons val collected))
        (loop (+ count 1)))))

;; 验证结果包含 1, 4, 9, 16, 25（顺序可能不同，因为是多核并发）
(check (length collected) => 5)
(check (pair? (member 1 collected)) => #t)
(check (pair? (member 4 collected)) => #t)
(check (pair? (member 9 collected)) => #t)
(check (pair? (member 16 collected)) => #t)
(check (pair? (member 25 collected)) => #t)

;; 3. go-spawn 参数错误路径（覆盖携带 RAII 对象的 raise 路径，Windows 回归）
(check-catch 'value-error (g_go-spawn '(a) '() '(begin)))
(check-catch 'type-error (g_go-spawn '(a) '(1) (lambda (x) x)))

(check-report)

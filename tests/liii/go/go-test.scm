(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go
;; 将一段代码调度到后台 worker 线程并发执行（每个 worker 独占一个 s7 会话）。
;;
;; 语法
;; ----
;; (go (fn arg ...))              ; 直接调用形式：ship fn 源码到 worker 执行
;; (go (captured-var ...) body ...)
;; (go body ...)
;;
;; 参数
;; ----
;; fn : procedure
;; 顶层 define 命名的函数。运行时经 procedure-source 提取源码运输到 worker，
;; worker 侧自动 import 全部 R7RS 库，因此 fn 体内可直接调用库函数。
;; arg 在主线程求值、序列化后作为实参传入，因此词法变量可直接作为实参。
;;
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
;; 2. 捕获变量与 arg 只支持可序列化的数据类型，不支持过程/闭包（spawn 时抛 type-error）。
;; 3. body 中可直接引用全局函数名（如 car、display），无需捕获。
;; 4. worker 内的运行时异常不会使线程池崩溃，错误输出到 stderr。
;;
;; 已知限制（现阶段不予处理）
;; ----
;; 1. (go (fn arg ...)) 形式只 ship fn 自身的源码，不递归 ship fn 体内引用的
;;    用户自定义辅助函数，也不处理 fn 对自身的递归引用——这两类引用在 worker
;;    中未绑定，任务报 unbound-variable（stderr）且主线程接收端死等。
;;    需要辅助函数时请改用库函数，或在 body 内联定义。
;; 2. 闭包的词法自由变量（如 (let ((n 10)) (lambda (x) (+ x n))) 中的 n）
;;    同样不会被运输。请把这类值改为显式形参传入。
;; 3. (go (fn arg ...) body ...) 多形式会按旧捕获列表语法解释，head 为过程时
;;    spawn 点报 type-error。请避免混用两种语法。

;; 1. 基本 go 任务启动与通道通信

(define (send-hello ch)
  (chan-send! ch "hello from worker" 5000)
) ;define

(define ch1 (make-chan 1))
(go (send-hello ch1))

(define msg1 (chan-recv! ch1 5000))
(check msg1 => "hello from worker")

;; 2. 多个 worker 并发向同一个 channel 写入计算结果

(define results (make-chan 10))

(define (square-worker ch i)
  (chan-send! ch (* i i) 5000)
) ;define

(define (spawn-worker i)
  (go (square-worker results i))
) ;define

;; 启动 5 个并发任务
(spawn-worker 1)
(spawn-worker 2)
(spawn-worker 3)
(spawn-worker 4)
(spawn-worker 5)

;; 收集所有结果

(define collected '())
(let loop
  ((count 0))
  (if (< count 5)
    (let ((val (chan-recv! results 5000)))
      (set! collected (cons val collected))
      (loop (+ count 1))
    ) ;let
  ) ;if
) ;let

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

;; 4. 函数直接调用形式：(go (fn arg ...))

(define (my-add-worker ch a b)
  (chan-send! ch (+ a b) 5000)
) ;define

(define test-ch1 (make-chan 1))
(go (my-add-worker test-ch1 10 20))
(check (chan-recv! test-ch1 5000) => 30)

;; 函数体内部依赖主环境已导入的外部库（如 liii range）
(import (liii range))

(define (my-range-worker ch)
  (range-for-each (lambda (i) (chan-send! ch i 5000)) (numeric-range 1 4))
  (chan-close! ch)
) ;define

(define test-ch2 (make-chan 3))
(go (my-range-worker test-ch2))
(check (chan-recv! test-ch2 5000) => 1)
(check (chan-recv! test-ch2 5000) => 2)
(check (chan-recv! test-ch2 5000) => 3)
(check (chan-recv! test-ch2 5000) => (eof-object))

;; 异常路径：(go (fn arg ...)) 首项不是 procedure
(check-catch 'type-error (go ("not-a-func" 123)))

(check-report)

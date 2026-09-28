(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go
;; 将函数调用调度到后台 worker 线程并发执行（每个 worker 独占一个独立 s7 会话）。
;;
;; 语法
;; ----
;; (go (fn arg ...))
;;
;; 参数
;; ----
;; fn : procedure
;; 顶层命名的函数。运行时通过 procedure-source 提取源码运输到后台 worker 线程，
;; worker 侧自动 import 全部可用 R7RS 库，因此 fn 体内可自由调用库函数。
;;
;; arg ... : any
;; 传递给 fn 的参数。参数在主线程求值并经 GFValue 序列化深拷贝给 worker，
;; 必须为可序列化的数据类型（数字、字符串、符号、列表、vector、bytevector、channel 等），
;; 不支持过程或闭包（否则在 spawn 时抛 type-error）。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. worker 间完全通过 channel 传递数据与结果（深拷贝传输，彻底隔离 GC 堆，无 GIL 锁竞争）。
;; 2. worker 内未捕获的运行时异常不会使主进程或线程池崩溃，错误将输出到 stderr。
;; 3. 仅支持 (go (fn arg ...)) 直接调用语法，旧语法 (go (vars ...) body ...) 已取缔，
;;    传入非直接调用语法将抛 syntax-error。
;;
;; 使用限制
;; ----
;; 1. 不递归 ship 外部辅助函数：
;;    (go (fn arg ...)) 仅 ship 目标函数 fn 自身的源码，不递归 ship fn 外部引用的
;;    非库辅助函数。若 fn 依赖其他函数，请使用标准库函数，或在 fn 内部通过 letrec/define 内联定义。
;; 2. 不支持自递归调用自身名字：
;;    fn 源码在 worker 中以匿名 lambda 执行，内部直接调用自身的顶层名字会报 unbound-variable。
;;    如需递归，请在 fn 内部使用 letrec 形式。
;; 3. 不支持捕获外部环境的词法自由变量：
;;    如 (let ((x 10)) (define (f ch) ... x ...) (go (f ch)))，闭包中的 x 无法被自动运输。
;;    所有外部变量必须作为实参显式传入：(go (f ch x))。

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

;; 异常路径：旧语法被取缔，非直接调用形式抛 syntax-error
(check-catch 'syntax-error (eval '(go (a) (+ a 1))))
(check-catch 'syntax-error (eval '(go (a b) (display a))))
(check-catch 'syntax-error (eval '(go 123)))
(check-catch 'syntax-error (eval '(go)))

(check-report)

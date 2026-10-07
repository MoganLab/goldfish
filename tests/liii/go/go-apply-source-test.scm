(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go-apply/source
;; go-apply 的源码级变体：直接以过程源码表达式（而非过程对象）投递任务，
;; 自由变量在调用方给定的捕获环境中解析。供需要把多个过程的来源组合成
;; 一个 worker 的调用方（如 (liii par) 的分块 worker）使用，避免在主会话
;; eval 构造包装过程。
;;
;; 语法
;; ----
;; (go-apply/source src env args ch)
;;
;; 参数
;; ----
;; src : list | symbol
;; 过程源码表达式（lambda 列表），或解析为过程的全局符号。
;;
;; env : let
;; 自由变量的捕获环境（通常是定义点的 funclet 或其派生 inlet）。
;;
;; args : list
;; 传给该过程的参数列表，元素须为可序列化的数据。
;;
;; ch : channel
;; 结果 channel，任务结束（无论正常返回还是抛出异常）后收到恰好一个结果对象。
;;
;; 返回值
;; ----
;; unspecified

;; 1. 基本调用：lambda 源码直接在 worker 执行

(define ch1 (make-chan 1))
(go-apply/source '(lambda (x) (* x x)) (rootlet) '(5) ch1)
(check (chan-recv! ch1 2000) => '(ok 25))

;; 2. 从给定环境捕获自由变量

(define ch2 (make-chan 1))
(let ((base 100))
  (go-apply/source '(lambda (x) (+ x base)) (funclet (lambda () base)) '(5) ch2)
) ;let
(check (chan-recv! ch2 2000) => '(ok 105))

;; 3. 派生 inlet 中的额外绑定也会被捕获

(define ch3 (make-chan 1))
(let ((extra (inlet 'offset 7)))
  (go-apply/source '(lambda (x) (+ x offset)) extra '(1) ch3)
) ;let
(check (chan-recv! ch3 2000) => '(ok 8))

;; 4. 异常传递：接收端收到 (error tag args)

(define ch4 (make-chan 1))
(go-apply/source '(lambda (x) (error 'value-error "boom" x)) (rootlet) '(42) ch4)
(check (chan-recv! ch4 2000) => '(error value-error ("boom" 42)))

;; 5. 参数类型校验（type-error）

(define ch5 (make-chan 1))
(check-catch 'type-error (go-apply/source 123 (rootlet) '(1) ch5))
(check-catch 'type-error (go-apply/source '(lambda (x) x) "not-an-env" '(1) ch5))
(check-catch 'type-error (go-apply/source '(lambda (x) x) (rootlet) "not-a-list" ch5))
(check-catch 'type-error (go-apply/source '(lambda (x) x) (rootlet) '(1) "not-a-chan"))

(check-report)

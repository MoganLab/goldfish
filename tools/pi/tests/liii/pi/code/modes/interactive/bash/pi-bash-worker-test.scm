;; pi-bash-worker
;; pi-bash-worker 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-bash-worker bash-jobs-chan event-chan abort-chan)
;;
;; 参数
;; ----
;; bash-jobs-chan : channel
;; ! 命令任务通道，元素为 (id command excluded?)。
;; event-chan : channel
;; 事件回传通道，bash_complete 事件由此送回主线程。
;; abort-chan : channel
;; ESC 中止信号通道：worker 流式轮询点探测，命中即杀进程树并结算退出码 -2。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii go)
  (liii time)
  (scheme time)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report)

;; ============================================================
;; 真实 go worker 路径（不注入 bash-handler）：
;; make-pi-app 生产路径即 (go (pi-bash-worker jobs-ch ev-ch abort-ch))，
;; worker 会话经 procedure-source 重求值，只能看到各库导出绑定。
;; ============================================================

;; 1. worker 能正常执行命令并回传 bash_complete 事件
(let* ((jobs-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (_ (go (pi-bash-worker jobs-ch ev-ch abort-ch)))
       (_2 (chan-send! jobs-ch (list 1 "echo hello" #f) 1000))
       (ev (chan-recv! ev-ch 10000 #f))
      ) ;
  (check-true (pair? ev))
  (when (pair? ev)
    (check (car ev) => 'bash_complete)
    (check (cadr ev) => 1)
    (check (caddr ev) => "echo hello")
    (check-true (string-contains? (cadddr ev) "hello"))
    (check (list-ref ev 4) => 0)
  ) ;when
  (chan-close! jobs-ch)
) ;let*

;; 2. ESC 中止：sleep 30 的长命令运行 0.5s 后发 abort 信号，
;; worker 轮询点探测后杀进程树并回传 bash_complete（退出码 -2），
;; 已产生的输出保留并追加 Command aborted。
;; 等待上限取 30s：CI 容器负载高时杀进程树可能明显变慢，
;; 上限过紧会把环境波动误判为功能失败（断言仍要求明显早于 sleep 30 结束）
(let* ((jobs-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (_ (go (pi-bash-worker jobs-ch ev-ch abort-ch)))
       (_2 (go-call (lambda (ch) (sleep 0.5) (chan-send! ch 'abort 0)) abort-ch))
       (_3 (chan-send! jobs-ch (list 2 "echo partial; sleep 30" #f) 1000))
       (ev (chan-recv! ev-ch 30000 #f))
      ) ;
  (check-true (pair? ev))
  (when (pair? ev)
    (check (car ev) => 'bash_complete)
    (check (cadr ev) => 2)
    (check (list-ref ev 4) => -2)
    (check-true (string-contains? (cadddr ev) "partial"))
    (check-true (string-contains? (cadddr ev) "Command aborted"))
  ) ;when
  (chan-close! jobs-ch)
) ;let*

;; 3. 任务开始前排空残留信号：空闲期到达的 abort 信号不误伤下一个命令
(let* ((jobs-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (_ (go (pi-bash-worker jobs-ch ev-ch abort-ch)))
       ;; 先发一个信号（此时无任务在运行，模拟上一条命令结算完才按 ESC）
       (_2 (chan-send! abort-ch 'abort 0))
       (_3 (chan-send! jobs-ch (list 3 "echo ok" #f) 1000))
       (ev (chan-recv! ev-ch 10000 #f))
      ) ;
  (check-true (pair? ev))
  (when (pair? ev)
    (check (car ev) => 'bash_complete)
    (check (list-ref ev 4) => 0)
    (check-true (string-contains? (cadddr ev) "ok"))
  ) ;when
  (chan-close! jobs-ch)
) ;let*

(check-report)

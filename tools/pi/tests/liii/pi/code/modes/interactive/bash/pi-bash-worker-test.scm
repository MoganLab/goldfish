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
  (liii os)
  (liii path)
  (liii time)
  (scheme time)
  (liii pi code modes interactive bash)
) ;import

(check-set-mode! 'report)

;; 等待工作目录下的相对路径文件出现（上限 limit-secs 秒），返回是否出现。
;; 用于把「命令已启动」与「发送 abort 信号」对齐：命令启动后才发信号，
;; abort 不会早于 worker 取出任务到达（否则会被当作空闲期残留信号排空）。

(define (%wait-file-appears rel-path limit-secs)
  (let ((deadline (+ (current-second) limit-secs)))
    (let loop
      ()
      (cond
       ((path-exists? (path-join (path-cwd) rel-path)) #t)
       ((>= (current-second) deadline) #f)
       (else (sleep 0.05) (loop))
      ) ;cond
    ) ;let
  ) ;let
) ;define

;; 标记文件名（相对工作目录，跨平台安全）：命令启动时创建，测试据此同步

(define MARKER-NAME
  (string-append "gf-pi-bash-worker-" (number->string (getpid)) ".marker")
) ;define

;; 清理上一轮遗留的标记文件（避免误判为本次命令已启动）
(when (path-exists? (path-join (path-cwd) MARKER-NAME))
  (path-unlink (path-join (path-cwd) MARKER-NAME))
) ;when

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

;; 2. ESC 中止：sleep 30 的长命令启动后发 abort 信号，
;; worker 轮询点探测后杀进程树并回传 bash_complete（退出码 -2），
;; 已产生的输出保留并追加 Command aborted。
;;
;; 同步方式：命令启动时创建标记文件，测试等到标记出现才发 abort 信号。
;; 固定延时不可靠——CI 负载高时 worker 线程启动（导入库）可达秒级，
;; abort 若早于 worker 取出任务到达会被当作空闲期残留信号排空，
;; 命令将跑满 sleep 30，结算事件收不到。
;; 等待上限取 60s（worker 启动）/30s（结算）：CI 容器负载高时
;; 杀进程树可能明显变慢，上限过紧会把环境波动误判为功能失败
(let* ((jobs-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (abort-ch (make-chan 1))
       (cmd (string-append "echo partial; : > '" MARKER-NAME "'; sleep 30"))
       (_ (go (pi-bash-worker jobs-ch ev-ch abort-ch)))
       (_2 (chan-send! jobs-ch (list 2 cmd #f) 1000))
       (_3 (%wait-file-appears MARKER-NAME 60))
       (t0 (current-second))
       (_4 (chan-send! abort-ch 'abort 0))
       (ev (chan-recv! ev-ch 30000 #f))
      ) ;
  (check-true (pair? ev))
  (when (pair? ev)
    (check (car ev) => 'bash_complete)
    (check (cadr ev) => 2)
    (check (list-ref ev 4) => -2)
    (check-true (string-contains? (cadddr ev) "partial"))
    (check-true (string-contains? (cadddr ev) "Command aborted"))
    ;; ESC 后应尽快结算（杀进程树 + 回传事件），不应跑满 sleep 30
    (check-true (< (- (current-second) t0) 20))
  ) ;when
  (when (path-exists? (path-join (path-cwd) MARKER-NAME))
    (path-unlink (path-join (path-cwd) MARKER-NAME))
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

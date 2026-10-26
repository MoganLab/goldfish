(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii list)
  (liii time)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app))

(check-set-mode! 'report)

;; ============================================================
;; 端到端回归（87bd7f16 拆分 app-events 后引入的 bug）：
;; ! 命令经真实 go worker 执行，主线程 pi-app-poll-events! 处理
;; bash_complete 回填卡片。app-events.scm 若漏 import bash 库，
;; bash-card-set-output! 解析失败抛 unbound variable，TUI 顶层
;; catch 捕获后整个进程退出（表现：卡在 Running 后闪退）。
;;
;; 注意：本测试刻意【不 import bash 库】，与真实 TUI 的 cli 入口
;; 环境一致——若在顶层 import bash 库会污染全局环境，掩盖 unbound。
;; ============================================================

(let* ((vt (make-virtual-terminal 80 24))
       (app (make-pi-app :terminal vt
              ;; 注入同步 handler 防止派生 agent worker；
              ;; 不传 :bash-handler —— 必须走真实 go worker 路径
              :handler (lambda (a m) #t)
              :record-sink (lambda (cmd text code) #f))
       ))

  ;; 1. 提交 ! 命令：卡片立即上屏（Running 态），任务进 worker
  (pi-app-run-bash! app "echo hello")

  ;; 2. 轮询主线程事件处理直至卡片完成（最多 ~10s）；
  ;;    app-events 漏 import bash 库时，pi-app-poll-events! 直接抛错
  (define (chat-lines)
    (apply append (map (lambda (c) (component-render c 80)) (pi-app-chat app)))
  ) ;define
  (define (running?)
    (let loop ((ls (map strip-ansi (chat-lines))))
      (cond ((null? ls) #f)
            ((string-contains? (car ls) "Running") #t)
            (else (loop (cdr ls)))
      ) ;cond
    ) ;let
  ) ;define
  (let wait ((n 0))
    (when (and (< n 100) (running?))
      (pi-app-poll-events! app)
      (sleep 0.1)
      (wait (+ n 1))
    ) ;when
  ) ;let

  ;; 3. 断言：卡片已回填 —— 无 Running，输出含 hello
  ;;    （退出码 0 为 complete 态，设计上不显示状态行）
  (let ((lines (map strip-ansi (chat-lines))))
    (check-true (not (any (lambda (l) (string-contains? l "Running")) lines)))
    (check-true (any (lambda (l) (string-contains? l "hello")) lines))
  ) ;let
) ;let*

(check-report)

(import (scheme base)
  (scheme write)
  (liii check)
  (liii string)
  (liii go)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; ============================================================
;; 真实 go worker 路径（不注入 bash-handler）：
;; make-pi-app 生产路径即 (go (pi-bash-worker jobs-ch ev-ch))，
;; worker 会话经 procedure-source 重求值，只能看到各库导出绑定。
;; ============================================================

;; 1. worker 能正常执行命令并回传 bash_complete 事件
(let* ((jobs-ch (make-chan 4))
       (ev-ch (make-chan 4))
       (_ (go (pi-bash-worker jobs-ch ev-ch)))
       (_2 (chan-send! jobs-ch (list 1 "echo hello" #f) 1000))
       (ev (chan-recv! ev-ch 10000 #f)))
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

(check-report)

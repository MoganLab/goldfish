(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; go 任务 fiber 化（方案 C）验收测试：任务级阻塞。
;; 跑法：GOLDFISH_GO_WORKERS=1 bin/gf tests/liii/go/worker-fiber-test.scm
;; （强制单 worker，保证任务落在同一 worker 会话）

;; 1. 跨调度周期唤醒：任务1挂在空通道上 → 调度器返回、worker 执行任务2 →
;;    任务2 送数触发 gate fire → 任务1 被唤醒并回传结果
(let ((ch (make-chan 1)) (res (make-chan 1)))
  (go (ch res) (chan-send! res (chan-recv! ch)))
  (go (ch) (g_msleep 200) (chan-send! ch 42))
  (check (chan-recv! res 5000 'timeout) => 42)
) ;let

;; 2. 真死锁仍报错：worker 任务内 fiber 全部阻塞在 fiber-chan 上且无外部
;;    唤醒源，调度器应抛 deadlock（经 catch 传回验证）
(let ((res (make-chan 1)))
  (go (res)
    (catch #t
      (lambda ()
        (spawn-fiber (lambda () (fiber-recv! (make-fiber-chan))))
        (fiber-scheduler-run!)
      ) ;lambda
      (lambda (tag args) (chan-send! res tag))
    ) ;catch
  ) ;go
  (check (chan-recv! res 5000 'timeout) => 'deadlock)
) ;let

;; 3. 异常隔离：任务1抛异常只走 stderr，不影响同 worker 的后续任务
(let ((res (make-chan 1)))
  (go () (car '()))
  (go (res) (chan-send! res 'alive))
  (check (chan-recv! res 5000 'timeout) => 'alive)
) ;let

;; 4. 无限等待形态挂起（fiber 化）不影响有限超时语义：超时返回 default
(let ((res (make-chan 1)))
  (go (res)
    (chan-send! res
      (list (chan-recv! (make-chan 1) 100 'to) (chan-send! (make-chan 0) 'x 100))
    ) ;chan-send!
  ) ;go
  (check (chan-recv! res 5000 'timeout) => '(to #f))
) ;let

;; 5. fiber↔fiber 无缓冲 rendezvous（跨任务）：go_bug.scm 场景——
;;    挂起的 recv watcher 与带载荷的 send watcher 直接交接
(let ((blocker (make-chan 0))
      (res (make-chan 1)))
  (go (blocker) (chan-recv! blocker))
  (go (blocker res)
    (chan-send! blocker 'wake)
    (chan-send! res 'rescued)
  ) ;go
  (check (chan-recv! res 5000 'timeout) => 'rescued)
) ;let

;; 6. 同场景的直接调用形式（go_bug.scm 用的形式）
(define (wf-block blocker)
  (chan-recv! blocker))
(define (wf-rescue blocker res)
  (chan-send! blocker 'wake)
  (chan-send! res 'rescued))
(let ((blocker (make-chan 0))
      (res (make-chan 1)))
  (go (wf-block blocker))
  (go (wf-rescue blocker res))
  (check (chan-recv! res 5000 'timeout) => 'rescued)
) ;let

(check-report)

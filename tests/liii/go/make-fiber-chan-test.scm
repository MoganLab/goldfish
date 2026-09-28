(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; make-fiber-chan
;; 创建一个 fiber 专用通道（单会话内的协程间通信）。
;;
;; 语法
;; ----
;; (make-fiber-chan)
;;
;; 返回值
;; ----
;; fiber-chan
;; 新的 fiber 通道对象。
;;
;; 说明
;; ----
;; fiber 通道是无界缓冲语义：fiber-send! 永不阻塞；
;; fiber-recv! 在无数据时挂起当前协程（不占用物理线程）。
;; fiber 通道仅在当前 s7 会话内有效，不能跨 worker 传输。

(define fch (make-fiber-chan))
(check (fiber-chan? fch) => #t)

(check-report)

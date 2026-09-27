(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. 测试一阶自引用环形列表 (Ouroboros cons cell)
(define ch1 (make-chan 1))
(define c1 (cons 42 '()))
(set-cdr! c1 c1) ; c1 指向自己

(chan-send! ch1 c1)
(define recv1 (chan-recv! ch1 1000))

(check (pair? recv1) => #t)
(check (car recv1) => 42)
(check (eq? (cdr recv1) recv1) => #t) ; 必须完全自引用！

;; 2. 测试三节点循环列表 (1 -> 2 -> 3 -> 1)
(define ch2 (make-chan 1))
(define c2 (list 1 2 3))
(set-cdr! (cddr c2) c2)

(chan-send! ch2 c2)
(define recv2 (chan-recv! ch2 1000))

(check (car recv2) => 1)
(check (cadr recv2) => 2)
(check (caddr recv2) => 3)
(check (eq? (cdddr recv2) recv2) => #t) ; 经过 3 步后必须环回自身！
(check (car (cdddr recv2)) => 1)

;; 3. 测试包含自引用的 Vector
(define ch3 (make-chan 1))
(define v (vector 100 200 #f))
(vector-set! v 2 v) ; v 的第 2 项引用自身

(chan-send! ch3 v)
(define recv3 (chan-recv! ch3 1000))

(check (vector? recv3) => #t)
(check (vector-ref recv3 0) => 100)
(check (vector-ref recv3 1) => 200)
(check (eq? (vector-ref recv3 2) recv3) => #t) ; 必须引用自身！

;; 4. 测试大块 Bytevector (1MB) 的高效传输
(define ch4 (make-chan 1))
(define big-bv (make-bytevector 1048576 170)) ; 1MB 填充 0xAA
(bytevector-u8-set! big-bv 0 1)
(bytevector-u8-set! big-bv 1048575 255)

(chan-send! ch4 big-bv)
(define recv4 (chan-recv! ch4 1000))

(check (bytevector? recv4) => #t)
(check (bytevector-length recv4) => 1048576)
(check (bytevector-u8-ref recv4 0) => 1)
(check (bytevector-u8-ref recv4 500000) => 170)
(check (bytevector-u8-ref recv4 1048575) => 255)

;; 5. 回归测试：同一个 Bytevector 在复合结构中被共享引用，反序列化后不能退化为 nil
(define ch5 (make-chan 1))
(define shared-bv (bytevector 7 8 9))
(chan-send! ch5 (list shared-bv shared-bv))
(define recv5 (chan-recv! ch5 1000))

(check (bytevector? (car recv5)) => #t)
(check (bytevector? (cadr recv5)) => #t)
(check (eq? (car recv5) (cadr recv5)) => #t)
(check (bytevector-u8-ref (cadr recv5) 0) => 7)

;; 6. 回归测试：共享 let 的 DAG 传输，两个引用必须指向同一对象
(define ch6 (make-chan 1))
(define shared-let (inlet 'x 42))
(chan-send! ch6 (list shared-let shared-let))
(define recv6 (chan-recv! ch6 1000))

(check (let? (car recv6)) => #t)
(check (let? (cadr recv6)) => #t)
(check ((car recv6) 'x) => 42)
(check (eq? (car recv6) (cadr recv6)) => #t)

;; 7. 回归测试：自引用 let（环），self 槽必须指回 let 自身
(define ch7 (make-chan 1))
(define cyc-let (inlet 'name "cyc" 'self #f))
(let-set! cyc-let 'self cyc-let)
(chan-send! ch7 cyc-let)
(define recv7 (chan-recv! ch7 1000))

(check (let? recv7) => #t)
(check (recv7 'name) => "cyc")
(check (eq? (recv7 'self) recv7) => #t)

;; 8. 回归测试：10 万层深列表传输（曾导致 C++ 递归序列化栈溢出段错误）
(define deep-list
  (let loop ((i 0) (acc '()))
    (if (< i 100000)
      (loop (+ i 1) (cons i acc))
      acc)))
(define ch8 (make-chan 1))
(chan-send! ch8 deep-list)
(define recv8 (chan-recv! ch8 30000))
(check (length recv8) => 100000)
(check (car recv8) => 99999)

;; 9. 回归测试：深层嵌套 vector
;; 注：深度受 s7 自身 GC 标记对嵌套 vector 的递归限制（小栈平台），5000 层足以验证迭代式序列化
(define deep-vec
  (let loop ((i 0) (v (vector 'leaf)))
    (if (< i 5000)
      (loop (+ i 1) (vector v))
      v)))
(define ch9 (make-chan 1))
(chan-send! ch9 deep-vec)
(define recv9 (chan-recv! ch9 30000))
(define innermost
  (let loop ((i 0) (v recv9))
    (if (< i 5000)
      (loop (+ i 1) (vector-ref v 0))
      v)))
(check (vector-ref innermost 0) => 'leaf)

(check-report)

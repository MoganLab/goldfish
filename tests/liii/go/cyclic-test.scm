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

(check-report)

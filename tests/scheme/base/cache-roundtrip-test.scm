(import (liii check)
        (liii string))

;; 本次缓存在线修复的回归测试：
;; 1. reader 对 s7 write 输出的复数/虚数字面量 round-trip
;;    (s7 write 无符号虚数为 "2i"，R7RS reader 需能读回)
;; 2. cacheable-expansion? 对 letrec 递归引用的判断
;; 3. 缓存 write-readable 对特殊符号的 |...| 转义

;; reader 复数/虚数 round-trip
(let ((p (open-input-string "2i")))
  (check (read p) => 0.0+2.0i))
(let ((p (open-input-string "-2.5i")))
  (check (read p) => 0.0-2.5i))
(let ((p (open-input-string "1.0+1.0i")))
  (check (read p) => 1.0+1.0i))

;; reader 特殊符号 round-trip
(let* ((s (string->symbol "hello'"))
       (p (open-output-string)))
  (write-readable `(a ,s) p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (cadr v) => s)))

;; cacheable-expansion? letrec 递归：
;; letrec 值表达式内引用自身（lowered core 常见形态），
;; cacheable-expansion? 不应误判为未绑定
(let ((sexp '(begin
               (define f
                 (lambda (x)
                   (letrec ((loop (lambda (i) (if (= i 0) 0 (+ i (loop (- i 1)))))))
                     (loop x)))))))
  (check (cacheable-expansion? sexp) => #t))

(check-report)

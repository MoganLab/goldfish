(import (liii check)
        (liii string))

;; read/write duality 与缓存 round-trip 的回归测试：
;; 1. reader 对复数/虚数字面量 round-trip（s7 write 无符号虚数为 "2i"）
;; 2. write-roundtrip 对特殊符号的 |...| 竖线转义
;; 3. write-roundtrip 对 record 的 #g 序列化（binding / toplevel-ref）
;; 4. write-roundtrip 对自引用 exp-library 的图标记（#n=/#n#）
;; 5. cacheable-expansion? 对 letrec 递归引用的判断

;; reader 复数/虚数 round-trip
(let ((p (open-input-string "2i")))
  (check (read p) => 0.0+2.0i))
(let ((p (open-input-string "-2.5i")))
  (check (read p) => 0.0-2.5i))
(let ((p (open-input-string "1.0+1.0i")))
  (check (read p) => 1.0+1.0i))

;; write-roundtrip 特殊符号 round-trip（竖线转义）
(let* ((s (string->symbol "hello'"))
       (p (open-output-string)))
  (write-roundtrip `(a ,s) p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (cadr v) => s)))

;; write-roundtrip 含空格符号 round-trip
(let* ((s (string->symbol "a b"))
       (p (open-output-string)))
  (write-roundtrip s p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (symbol? v) => #t)
    (check (symbol->string v) => "a b")))

;; write-roundtrip binding record round-trip
(let* ((b (make-primitive-binding 'foo))
       (p (open-output-string)))
  (write-roundtrip b p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (record-instance? v) => #t)
    (check (binding-kind v) => 'primitive)
    (check (binding-value v) => 'foo)))

;; write-roundtrip toplevel-ref record round-trip
(let* ((tr (make-toplevel-ref 'my-var:1 #f 'my-var #t))
       (p (open-output-string)))
  (write-roundtrip tr p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (record-instance? v) => #t)
    (check (toplevel-ref-gensym v) => 'my-var:1)
    (check (toplevel-ref-original v) => 'my-var)
    (check (toplevel-ref-exported? v) => #t)))

;; write-roundtrip 自引用 exp-library round-trip（图标记 #n=/#n#）
(let* ((lib (make-exp-library '(liii test)))
       (ref (make-toplevel-ref 'my-var:1 lib 'my-var #t))
       (p (open-output-string)))
  (exp-library-define! lib 'my-var (make-toplevel-binding ref))
  (write-roundtrip lib p)
  (let ((v (read (open-input-string (get-output-string p)))))
    (check (record-instance? v) => #t)
    (check (exp-library-name v) => '(liii test))
    (let ((b (exp-library-ref v 'my-var)))
      (check (binding? b) => #t)
      ;; toplevel-ref 的 home 应指回重建的库（循环恢复）
      (check (eq? (toplevel-ref-home (binding-value b)) v) => #t))))

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

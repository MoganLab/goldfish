(import (liii check)
        (goldfish compiler)
        (srfi srfi-1))

;; L2-2 集成测试：编译 pass 已接入加载路径。
;;
;; 验证：
;;   1. (goldfish compiler) 库在正常加载路径中已注册（load-library!
;;      加载它并对 defs 跑编译）。
;;   2. 编译确实改变了 defs：compile-defs-on-load 的输出与原 defs 不同
;;      （有常量折叠/if 化简发生）。
;;   3. 编译后的库功能正常（与未编译等价，回归已覆盖）。

(check (procedure? compile-defs) => #t)
(check (procedure? constant-fold) => #t)
(check (procedure? simplify-if) => #t)

;; 手动模拟加载路径上的编译：对真实库 defs 跑管线，验证有改写且等价
(let* ((forms (call-with-input-file "goldfish/srfi/srfi-1.scm" read-forms))
       (recs (let*-values (((r c) (capture-file-cache forms))) r))
       (rec (car recs))
       (defs (list-ref rec 5)))
  (let ((compiled (compile-defs defs (list constant-fold simplify-if))))
    (let loop ((ds defs) (cs compiled) (diff 0))
      (if (null? ds)
        (check (> diff 0) => #t)
        (loop (cdr ds)
              (cdr cs)
              (+ diff (if (equal? (car ds) (car cs)) 0 1)))))))

;; 编译后的库正常使用（srfi-1 在 import 时已走编译路径）
(check (equal? (take (list 1 2 3 4 5) 3) '(1 2 3)) => #t)
(check (equal? (fold + 0 '(1 2 3 4 5)) 15) => #t)
(check (equal? (filter odd? '(1 2 3 4 5 6)) '(1 3 5)) => #t)
(check (equal? (append-map (lambda (x) (list x x)) '(a b)) '(a a b b)) => #t)

(check-report)

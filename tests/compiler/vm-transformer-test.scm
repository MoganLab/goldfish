(import (liii check)
        (goldfish compiler)
        (goldfish)
        (srfi srfi-1)
        (srfi srfi-13))

;; 自研字节码 VM 回归测试：覆盖三类修复
;;   1. 非尾位置的 let/letrec 提前 return（bytecode.scm c752a66c）
;;   2. VM closure 作为 s7 map 回调被延迟 apply 返回空（goldfish_vm.cpp e64e48b2）
;;   3. procedural 宏 transformer 的 VM program 展开（deindent-impl 场景）

(eval-when (expand)
  (for-each (lambda (name)
              (exp-library-define! (program-library) name
                                   (make-primitive-binding name)))
            '(vm-non-tail vm-nested vm-map vm-map-capture vm-deindent vm-fe vm-cwv)))

(define (vm-load-defs defs)
  (vm-load (to-bytecode (map core->ir defs)) #f))

;; ===== 1. 非尾位置的 let（绑定值含 letrec）=====
;; 修复前 compile-let 在非尾位置也 emit (return)，绑定值里的 letrec
;; 提前返回 loop 闭包而非完成 body。
(vm-load-defs '((define (vm-non-tail x)
                  (let ((a (letrec ((loop (lambda (i acc)
                                            (if (= i 0) acc (loop (- i 1) (cons i acc))))))
                             (loop x '()))))
                    (append a (list x))))))

(check (vm-non-tail 3) => '(1 2 3 3))
(check (vm-non-tail 0) => '(0))

;; 嵌套非尾 let：值必须在栈上，不能被外层 return 提前弹出
(vm-load-defs '((define (vm-nested x)
                  (let ((a (+ x 1)))
                    (let ((b (* a 2)))
                      (list a b))))))

(check (vm-nested 5) => '(6 12))
(check (vm-nested 0) => '(1 2))

;; ===== 2. VM closure 作为 s7 map 回调 =====
;; 修复前 s7 的 map 对无法 cell-optimize 的 VM closure 壳走 OP_MAP_2
;; 延迟路径，返回 unspecified，收集值丢失（结果为 ()）。
(vm-load-defs '((define (vm-map xs)
                  (map (lambda (x) (* x 2)) xs))))

(check (vm-map '(1 2 3)) => '(2 4 6))
(check (vm-map '()) => '())
(check (vm-map '(7)) => '(14))

;; 多元素 + 捕获外层变量（闭包捕获经 map 回调）
(vm-load-defs '((define (vm-map-capture xs n)
                  (map (lambda (x) (+ x n)) xs))))

(check (vm-map-capture '(1 2) 10) => '(11 12))

;; ===== 3. deindent-impl 等价函数（procedural 宏 transformer 的 body）=====
;; 组合了 named-let loop、非尾 let*、module-ref 取 srfi 函数、map 回调，
;; 是 raw-string 库 deindent 宏在 VM program 下展开失败的完整复现。
;; 注意 defs 不经展开器，跨库的 srfi 函数用 module-ref 显式取出
;; （与展开器生成的 lowered 形式一致）。
(vm-load-defs '((define (vm-deindent str)
                  (let* ((lines (let loop ((start 1) (result '()))
                                  (let ((nl ((module-ref '(srfi srfi-13) 'string-index)
                                             str #\newline start (string-length str))))
                                    (if (not nl)
                                      (reverse (cons (substring str start (string-length str)) result))
                                      (loop (+ nl 1) (cons (substring str start nl) result))))))
                         (closing-line ((module-ref '(srfi srfi-1) 'last) lines))
                         (ref-indent ((module-ref '(srfi srfi-13) 'string-count)
                                      closing-line #\space))
                         (content-lines ((module-ref '(srfi srfi-1) 'drop-right) lines 1)))
                    ((module-ref '(srfi srfi-13) 'string-join)
                     (map (lambda (line)
                            (if ((module-ref '(srfi srfi-13) 'string-null?) line)
                              "" (substring line ref-indent)))
                          content-lines)
                     "\n")))))

(check (vm-deindent "\n  a\n  b\n  ") => "a\nb")
(check (vm-deindent "\n  a\n  ") => "a")
(check (vm-deindent "\n  SELECT *\n  FROM users\n  ") => "SELECT *\nFROM users")
(check (vm-deindent "\n  a b\n  c d\n  ") => "a b\nc d")

;; ===== 4. vm-load 直接加载 lambda（transformer 等价物）=====
;; 覆盖非尾 let + map 组合在闭包 body 里的执行。
(define tr-non-tail
  (vm-load (to-bytecode (list (core->ir '(lambda (x)
                                           (let* ((a (let loop ((i x) (acc '()))
                                                      (if (= i 0) acc (loop (- i 1) (cons i acc)))))
                                                  (b (length a)))
                                             (+ (length a) b))))))
           #f))

(check (tr-non-tail 3) => 6)   ; '(3 2 1) 长度 3 → 6
(check (tr-non-tail 1) => 2)   ; '(1) 长度 1 → 2

(define tr-map
  (vm-load (to-bytecode (list (core->ir '(lambda (xs)
                                           (map (lambda (x) (* x 3)) xs)))))
           #f))

(check (tr-map '(1 2 3)) => '(3 6 9))
(check (tr-map '(10)) => '(30))

;; ===== 6. VM closure body 内调用 s7 for-each =====
;; 回归：s7 的 g_for_each_closure 对单表达式 body（VM closure 壳
;; (vm-enter ...)）走 OP_FOR_EACH_2 延迟 apply 并返回 unspecified，
;; VM 的 Call 指令拿不到回调结果。set-size-test 经 (liii set) 的
;; (define (set . elements) ... (for-each ...) result) 触发。
;; 回调用 set-car! 原地修改计数器，验证回调实际执行了 N 次。
(vm-load-defs '((define (vm-fe x)
                  (let ((acc (list 0)))
                    (for-each (lambda (y) (set-car! acc (+ (car acc) 1))) x)
                    (car acc)))))

(check (vm-fe '(1 2 3)) => 3)
(check (vm-fe '()) => 0)
(check (vm-fe '(7)) => 1)

;; ===== 7. call-with-values 多值 producer =====
;; 回归：VM 的 CallWithValues 指令对非 VM 闭包的 producer（普通 s7
;; lambda）返回的多值解包错误——s7 在 apply_function 里把 (values 1 2)
;; splice 后清除多值标记，VM 的 is_multiple_value 检测不到，把 (1 2)
;; 当单个参数传给 consumer。json 库的 string->json 用 call-with-values
;; 接收 handle-escape-char 的多值，触发该 bug。
(vm-load-defs '((define (vm-cwv p c)
                  (call-with-values p c))))

(check (vm-cwv (lambda () (values 1 2)) +) => 3)
(check (vm-cwv (lambda () (values 1 2)) list) => '(1 2))
(check (vm-cwv (lambda () (values 3 4)) +) => 7)

(check-report)

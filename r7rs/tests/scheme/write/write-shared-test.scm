;; write-shared
;; 写出对象的外部表示，用 datum 标签表示所有共享与循环结构。
;;
;; 语法
;; ----
;; (write-shared obj)
;; (write-shared obj port)
;;
;; 参数
;; ----
;; obj : any
;; 要输出的对象。
;;
;; port : output-port? (可选)
;; 输出端口。省略时，写入当前输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 描述
;; ----
;; R7RS 6.13：write-shared 与 write 类似，但用 datum 标签
;; （#n= / #n#）表示全部共享与循环结构，读回后保持结构共享。
;; 1. 共享（非循环）结构：首次出现输出 #n= 定义，再次出现输出 #n#。
;; 2. 循环结构：回边目标贴标签，输出形如 #1=(1 2 . #1#)。
;; 3. 非共享非循环结构：与 write 一致。
;; 4. 两遍实现（先扫描全图统计引用与回边，再输出），输出形态与
;;    Guile 3 逐字节对齐；原子类型（数字、字符串等）不参与标注。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (scheme write) (scheme base))
(check-set-mode! 'report-failed)

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define

(check-true (procedure? write-shared))

;; 基础类型：与 write 一致
(check (capture-output (lambda (port) (write-shared '(a b) port))) => "(a b)")
(check (capture-output (lambda (port) (write-shared "goldfish" port)))
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port) (write-shared 456 port))) => "456")
(check (capture-output (lambda (port) (write-shared '(1 (2 3)) port)))
  =>
  "(1 (2 3))"
) ;check

;; 共享（非循环）结构：必须标注
(check
  (capture-output
    (lambda (port)
      (let ((a (list 1 2))) (write-shared (list a a) port))
    ) ;lambda
  ) ;capture-output
  =>
  "(#1=(1 2) #1#)"
) ;check

;; 共享的 cdr 尾
(check
  (capture-output
    (lambda (port)
      (let* ((d (list 'tail))
             (e (list 'a d))
             (f (list 'b d)))
        (write-shared (list e f) port)
      ) ;let*
    ) ;lambda
  ) ;capture-output
  =>
  "((a #1=(tail)) (b #1#))"
) ;check

;; 共享点对
(check
  (capture-output
    (lambda (port)
      (let ((tl (cons 'x 'y))) (write-shared (list tl tl) port))
    ) ;lambda
  ) ;capture-output
  =>
  "(#1=(x . y) #1#)"
) ;check

;; 共享 vector
(check
  (capture-output
    (lambda (port)
      (let ((v (vector 1))) (write-shared (list v v) port))
    ) ;lambda
  ) ;capture-output
  =>
  "(#1=#(1) #1#)"
) ;check

;; 嵌套共享：内层标签先于外层引用
(check
  (capture-output
    (lambda (port)
      (let* ((b (list 'x)) (c (list b b)))
        (write-shared (list c c) port)
      ) ;let*
    ) ;lambda
  ) ;capture-output
  =>
  "(#1=(#2=(x) #2#) #1#)"
) ;check

;; 循环列表：回边目标贴标签
(check
  (capture-output
    (lambda (port)
      (let ((x (list 1 2))) (set-cdr! (cdr x) x) (write-shared x port))
    ) ;lambda
  ) ;capture-output
  =>
  "#1=(1 2 . #1#)"
) ;check

;; 自共享 vector
(check
  (capture-output
    (lambda (port)
      (let ((sv (vector 1 #f)))
        (vector-set! sv 1 sv)
        (write-shared sv port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "#1=#(1 #1#)"
) ;check

;; 共享 + 循环混合
(check
  (capture-output
    (lambda (port)
      (let* ((g1 (list 1)) (g2 (list g1 g1)))
        (set-cdr! (cdr g2) g2)
        (write-shared g2 port)
      ) ;let*
    ) ;lambda
  ) ;capture-output
  =>
  "#1=(#2=(1) #2# . #1#)"
) ;check

;; 原子类型不参与标注：相同字面量不贴标签
(check
  (capture-output (lambda (port) (write-shared (list 5 5 "a" "a") port)))
  =>
  "(5 5 \"a\" \"a\")"
) ;check

(check-report)

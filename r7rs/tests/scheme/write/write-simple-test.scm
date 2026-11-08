;; write-simple
;; 写出对象的外部表示，绝不生成 datum 标签。
;;
;; 语法
;; ----
;; (write-simple obj)
;; (write-simple obj port)
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
;; R7RS 6.13：write-simple 与 write 类似，但绝不使用 datum 标签
;; （#n= / #n#）表示共享或循环结构。
;; 1. 共享（非循环）结构与 write 一致：展开重复，不加标签。
;; 2. 循环结构输出占位符 #-1#（非规范 datum 标签，不可读回）并
;;    终止该分支，行为与 Guile 3 对齐；R7RS 允许 write-simple 对
;;    循环结构不终止，占位符是该许可下的确定性策略。
;; 3. 其余类型（数字、字符串、字符、符号等）输出与 write 一致。
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

(check-true (procedure? write-simple))

;; 基础类型：与 write 一致
(check
  (capture-output (lambda (port) (write-simple '(a b) port)))
  =>
  "(a b)"
) ;check
(check (capture-output (lambda (port) (write-simple "goldfish" port)))
  =>
  "\"goldfish\""
) ;check
(check (capture-output (lambda (port) (write-simple 123 port))) => "123")
(check
  (capture-output
    (lambda (port) (write-simple '(1 (2 3) . 4) port))
  ) ;capture-output
  =>
  "(1 (2 3) . 4)"
) ;check
(check (capture-output (lambda (port) (write-simple #(1 #(2)) port)))
  =>
  "#(1 #(2))"
) ;check
(check (capture-output (lambda (port) (write-simple #u8(1 2 3) port)))
  =>
  "#u8(1 2 3)"
) ;check

;; 共享（非循环）结构：展开重复，不加标签
(check
  (capture-output
    (lambda (port) (let ((a (list 1 2))) (write-simple (list a a) port)))
  ) ;capture-output
  =>
  "((1 2) (1 2))"
) ;check

;; 循环列表：cdr 循环 → 占位符并终止
(check
  (capture-output
    (lambda (port)
      (let ((x (list 1 2)))
        (set-cdr! (cdr x) x)
        (write-simple x port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "(1 2 . #-1#)"
) ;check

;; 循环列表：car 循环
(check
  (capture-output
    (lambda (port)
      (let ((x (list 1 2)))
        (set-car! (cdr x) x)
        (write-simple x port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "(1 #-1#)"
) ;check

;; 嵌套循环：内外两个循环点各自占位
(check
  (capture-output
    (lambda (port)
      (let ((z (list 0 (list 1 2))))
        (set-cdr! (cdr (cadr z)) (cadr z))
        (set-cdr! (cdr z) z)
        (write-simple z port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "(0 (1 2 . #-1#) . #-1#)"
) ;check

;; 循环向量
(check
  (capture-output
    (lambda (port)
      (let ((v (vector 1 2)))
        (vector-set! v 1 v)
        (write-simple v port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "#(1 #-1#)"
) ;check

;; 共享结构混合循环：共享不标注，循环占位
(check
  (capture-output
    (lambda (port)
      (let ((x (list 1 2)))
        (set-cdr! (cdr x) x)
        (write-simple (list x (list 'a x)) port)
      ) ;let
    ) ;lambda
  ) ;capture-output
  =>
  "((1 2 . #-1#) (a (1 2 . #-1#)))"
) ;check

(check-report)

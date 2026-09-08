(import (liii check) (liii vector))

(check-set-mode! 'report-failed)

;; 默认 vector 没有 typer
(let ((v (vector 1 2 3)))
  (check (vector-typer v) => #f)
) ;let

;; 设置 vector-typer 为 integer?
(let ((v (vector 1 2 3)))
  (set! (vector-typer v) integer?)
  (check (vector-typer v) => integer?)
  (vector-set! v 0 10)
  (check (vector-ref v 0) => 10)
  (check-catch 'wrong-type-arg (vector-set! v 0 "not-int"))
) ;let

;; 重置 vector-typer 为 #f
(let ((v (vector 1 2 3)))
  (set! (vector-typer v) integer?)
  (set! (vector-typer v) #f)
  (check (vector-typer v) => #f)
  (vector-set! v 0 "string-ok")
  (check (vector-ref v 0) => "string-ok")
) ;let

;; 同构向量的 vector-typer
(check (vector-typer (int-vector 1 2)) => integer?)
(check (vector-typer (float-vector 1.0 2.0)) => float?)
(check (vector-typer (byte-vector 1 2)) => byte?)
(check (vector-typer (complex-vector 1.0+2.0i)) => number?)

;; 错误情况
(check-catch 'wrong-type-arg (set! (vector-typer 'not-a-vector) integer?))
(check-catch 'wrong-type-arg
  (let ((v (vector 1 2)))
    (set! (vector-typer v) "not-a-func")
  ) ;let
) ;check-catch

(check-report)

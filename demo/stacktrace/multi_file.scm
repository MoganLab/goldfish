;; multi_file.scm: 多文件跨模块调用栈回溯演示
(load "demo/stacktrace/service.scm")

(define (process-user-record record)
  (cons "record" (format-score (car record) (cadr record) (caddr record)))
) ;define

(define (run-batch user-list)
  (cons "batch-result" (process-user-record (car user-list)))
) ;define

;; 入口：传入 base 为 0 的数据，触发 engine.scm 内部的除以零错误
(run-batch '(("alice" 100 0)))

;; run-one-task
;; 执行单个文件的任务，并把结果统一包装为 (路径 状态 失败信息) 三元表。
;; 任务过程抛出的异常由兜底过程接管，兜底过程需返回 (状态 失败信息)。
;;
;; 语法
;; ----
;; (run-one-task path unit-fn err-fn)
;;
;; 参数
;; ----
;; path : any
;; 任务对应的文件路径，原样写入结果表。
;; unit-fn : procedure?
;; 任务过程，接收文件路径，返回 (状态 失败信息)。
;; err-fn : procedure?
;; 兜底过程，接收异常标签与信息，返回 (状态 失败信息)。
;;
;; 返回值
;; ------
;; list
;; 形如 (path status msg) 的三元表。

(import (scheme base) (liii check) (liii goldtool parallel))

(check-set-mode! 'report-failed)

;; 正常执行：状态与信息来自任务过程
(check
  (run-one-task "a.scm" (lambda (p) (list 'ok "done")) (lambda (tag info) (list 'failed "boom")))
  =>
  '("a.scm" ok "done")
) ;check

;; 任务过程抛出异常时由兜底过程接管
(check
  (run-one-task "b.scm"
    (lambda (p) (error "boom"))
    (lambda (tag info) (list 'failed "fallback"))
  ) ;run-one-task
  =>
  '("b.scm" failed "fallback")
) ;check

;; 状态为 #f 表示失败，兜底信息被原样保留
(check
  (run-one-task "c.scm"
    (lambda (p) (list #f "syntax error"))
    (lambda (tag info) (list 'failed "fallback"))
  ) ;run-one-task
  =>
  '("c.scm" #f "syntax error")
) ;check

(check-report)

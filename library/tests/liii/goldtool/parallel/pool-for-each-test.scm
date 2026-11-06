;; pool-for-each
;; 以 worker 池并发执行文件级任务，结果按输入顺序返回，
;; 并在全部任务完成后按顺序逐条调用回调。
;;
;; 语法
;; ----
;; (pool-for-each file-fn files on-result)
;;
;; 参数
;; ----
;; file-fn : procedure?
;; 文件任务过程，接收文件路径，返回 (路径 状态 失败信息) 三元表。
;; files : list
;; 待处理的文件列表。
;; on-result : procedure?
;; 结果回调，接收 (文件 结果) 两个参数。
;;
;; 返回值
;; ------
;; list
;; 各文件的结果三元表列表，顺序与 files 一致；files 为空时返回空列表。

(import (scheme base) (liii check) (liii list) (liii goldtool parallel))

(check-set-mode! 'report-failed)

(define original-jobs (goldtool-jobs))

;; 串行执行，结果顺序与输入顺序一致
(set-goldtool-jobs! 1)
(check
  (pool-for-each
    (lambda (f) (list f 'ok "done"))
    '("a.scm" "b.scm")
    (lambda (f r) #t)
  ) ;pool-for-each
  =>
  '(("a.scm" ok "done") ("b.scm" ok "done"))
) ;check

;; 空文件列表返回空结果
(check (pool-for-each (lambda (f) (list f 'ok "")) '() (lambda (f r) #t)) => '())

;; 回调按结果顺序收到每个文件及其结果
(let ((seen '())) ;
  (pool-for-each
    (lambda (f) (list f 'ok "done"))
    '("a.scm" "b.scm")
    (lambda (f r) (set! seen (append seen (list (list f (cadr r))))))
  ) ;pool-for-each
  (check seen => '(("a.scm" ok) ("b.scm" ok)))
) ;let

;; 并发执行时结果同样按输入顺序返回
(set-goldtool-jobs! 4)
(check
  (pool-for-each
    (lambda (f) (list f 'ok "done"))
    '("a.scm" "b.scm" "c.scm")
    (lambda (f r) #t)
  ) ;pool-for-each
  =>
  '(("a.scm" ok "done") ("b.scm" ok "done") ("c.scm" ok "done"))
) ;check

;; 恢复原值，避免影响其它测试
(set-goldtool-jobs! original-jobs)

(check-report)

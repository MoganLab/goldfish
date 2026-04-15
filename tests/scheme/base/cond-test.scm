(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cond
;; 测试 cond 条件分支表达式。
;;
;; 语法
;; ----
;; (cond clause ...)
;;
;; 参数
;; ----
;; clause : cond 子句
;;
;; 返回值
;; ----
;; any?
;; 返回首个命中子句的结果。
;;
;; 注意
;; ----
;; 本文件覆盖普通子句和 => 子句两种形式。
;;
;; 示例
;; ----
;; (cond ((> 3 2) 3) (else 2)) => 3
;;
;; 错误处理
;; ----
;; 按命中子句中表达式自身规则处理
(check (cond ((> 3 2) ((lambda () 3)))
             (else ((lambda () 2)))
       ) ;cond
  =>
  3
) ;check
(check (cond ((< 3 2) ((lambda () 3)))
             (else ((lambda () 2)))
       ) ;cond
  =>
  2
) ;check
(check (cond ((> 3 2) 3) (else 2)) => 3)
(check (cond ((< 3 2) 3) (else 2)) => 2)
(check (cond ((and (> 3 1) (< 3 4)) 'true-branch)
             (else 'false-branch)
       ) ;cond
  =>
  'true-branch
) ;check
(check (cond ((or (> 3 4) (< 3 1)) 'true-branch)
             (else 'false-branch)
       ) ;cond
  =>
  'false-branch
) ;check
(check (cond (2 => (lambda (n) (* n 2)))) => 4)
(check (cond (#f => (lambda (n) (* n 2)))
             (else 'no-match)
       ) ;cond
  =>
  'no-match
) ;check
(check (cond (3 => (lambda (n) (* n 2)))
             (else 'no-match)
       ) ;cond
  =>
  6
) ;check
(check-report)
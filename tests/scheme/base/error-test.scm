(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; error
;; 抛出错误并中断执行。
;;
;; 语法
;; ----
;; (error message)
;; (error obj ...)
;;
;; 参数
;; ----
;; message : string?
;; 错误信息。
;; obj ... : 任意类型
;; 额外的错误相关对象。
;;
;; 返回值
;; ------
;; 不返回
;; 抛出错误，不返回正常值。
;;
;; 说明
;; ----
;; 1. 抛出可捕获的错误
;; 2. 通常用于表示程序遇到无法恢复的情况
;; 3. 可用 catch 或 guard 捕获处理
(check
  (catch #t
    (lambda () (error 'test-err) 'unreachable)
    (lambda args 'caught)
  ) ;catch
  => 'caught
) ;check
(check
  (catch #t
    (lambda () (error "message"))
    (lambda args 'caught-msg)
  ) ;catch
  => 'caught-msg
) ;check

(check-report)

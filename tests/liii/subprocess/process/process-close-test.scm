(import (liii check) (liii subprocess) (liii string))

(check-set-mode! 'report-failed)

;; process-close
;; 关闭子进程相关的文件句柄和资源。
;;
;; 语法
;; ----
;; (process-close proc)
;;
;; 参数
;; ----
;; proc : process
;; 进程对象。
;;
;; 返回值
;; ----
;; 未定义
;;
;; 说明
;; ----
;; 释放与该进程相关的管道、句柄等底层系统资源。

(let ((proc (process-spawn '("python3" "-c" "import sys; sys.exit(0)"))))
  (process-wait proc 2000)
  (let ((str (object->string proc)))
    (check (string-contains? str "#<process pid:") => #t)
  ) ;let
  (process-close proc)
) ;let

;; 类型错误
(check-catch 'type-error (process-close #t))
(check-catch 'type-error (process-close 42))

(check-report)

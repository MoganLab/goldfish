;; make-stderr-handler
;; 创建把日志输出到标准错误流的处理器。
;;
;; 语法
;; ----
;; (make-stderr-handler)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 接收一条日志 alist 的回调过程，输出到标准错误流。

(import (liii check) (liii logging) (liii string))

(check-set-mode! 'report-failed)

(check (procedure? (make-stderr-handler)) => #t)

;; 处理器把消息写入标准错误流
(let ((p (open-output-string)))
  (let ((old-err (set-current-error-port p)))
    (let ((handler (make-stderr-handler)))
      (handler '((SEVERITY . 4) (MESSAGE . "to stderr")))
    ) ;let
    (set-current-error-port old-err)
  ) ;let
  (check (string-contains? (get-output-string p) "to stderr") => #t)
) ;let

(check-report)

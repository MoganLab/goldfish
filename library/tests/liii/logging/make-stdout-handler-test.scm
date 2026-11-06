;; make-stdout-handler
;; 创建把日志输出到标准输出流的处理器。
;;
;; 语法
;; ----
;; (make-stdout-handler)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; procedure
;; 接收一条日志 alist 的回调过程，输出到标准输出流。

(import (liii check) (liii logging) (liii string))

(check-set-mode! 'report-failed)

(check (procedure? (make-stdout-handler)) => #t)

;; 处理器把消息写入标准输出流
(let ((p (open-output-string)))
  (let ((old-out (set-current-output-port p)))
    (let ((handler (make-stdout-handler)))
      (handler '((SEVERITY . 6) (MESSAGE . "to stdout")))
    ) ;let
    (set-current-output-port old-out)
  ) ;let
  (check (string-contains? (get-output-string p) "to stdout") => #t)
) ;let

(check-report)

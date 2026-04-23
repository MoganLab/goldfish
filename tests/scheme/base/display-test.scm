(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; display
;; 将对象以人类可读形式输出到端口。
;;
;; 语法
;; ----
;; (display obj)
;; (display obj port)
;;
;; 参数
;; ----
;; obj : 任意类型
;; 要输出的对象。
;; port : output-port?
;; 可选，默认为当前输出端口。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 字符串输出时不带引号
;; 2. 字符直接输出，不转义
(let ((p (open-output-string)))
  (display "hello" p)
  (check (get-output-string p) => "hello")
) ;let
(let ((p (open-output-string)))
  (display 123 p)
  (check (get-output-string p) => "123")
) ;let

(check-report)

;; pi-tool-result-message?
;; 判断对象是否为工具执行结果消息 record 实例。
;;
;; 语法
;; ----
;; (pi-tool-result-message? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具执行结果消息 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-result-message "call-001" "read" "文件内容" #f 5000)))
  (check-true (pi-tool-result-message? obj))
) ;let
(check-false (pi-tool-result-message? '()))
(check-false (pi-tool-result-message? #f))
(check-false (pi-tool-result-message? "not-a-record"))

(check-report)

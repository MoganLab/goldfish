;; %make-pi-tool-result
;; 构造工具执行结果 record 对象。
;;
;; 语法
;; ----
;; (%make-pi-tool-result content is-error)
;;
;; 参数
;; ----
;; content : any?
;; 工具输出内容
;;
;; is-error : boolean?
;; 是否为错误结果
;;
;; 返回值
;; ----
;; pi-tool-result?
;; 工具执行结果 record 实例。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-result "执行成功" #f)))
  (check-true (pi-tool-result? obj))
  (check (pi-tool-result-content obj) => "执行成功")
  (check (pi-tool-result-is-error obj) => #f)
) ;let

(check-report)

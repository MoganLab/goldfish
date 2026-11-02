;; pi-tool-result
;; 构造工具执行结果 record 对象。
;;
;; 语法
;; ----
;; (pi-tool-result content)
;; (pi-tool-result content is-error)
;;
;; 参数
;; ----
;; content : any?
;; 结果内容，可为字符串、vector 或 list。
;;
;; is-error : boolean?
;; 可选，是否为错误结果（默认为 #f）。
;;
;; 返回值
;; ----
;; pi-tool-result?
;; 工具结果 record 实例。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((res (pi-tool-result "hello" #f)))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-content res) => '#(((type . "text") (text . "hello"))))
  (check-false (pi-tool-result-is-error res))
) ;let

(check-report)

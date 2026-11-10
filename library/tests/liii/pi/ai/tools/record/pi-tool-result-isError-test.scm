;; pi-tool-result-isError
;; 获取工具执行结果是否为错误（驼峰命名别名）。
;;
;; 语法
;; ----
;; (pi-tool-result-isError res)
;;
;; 参数
;; ----
;; res : pi-tool-result?
;; 工具结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若该结果为错误则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai tools record))

(check-set-mode! 'report-failed)

(let ((ok-res (pi-tool-result "ok" #f)) (err-res (pi-tool-result "fail" #t)))
  (check (pi-tool-result-isError ok-res) => #f)
  (check (pi-tool-result-isError err-res) => #t)
) ;let

(check-report)

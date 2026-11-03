;; pi-tool-result-content
;; 获取工具执行结果 record 的 content 字段。
;;
;; 语法
;; ----
;; (pi-tool-result-content obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行结果 record 实例。
;;
;; 返回值
;; ----
;; any?
;; content 字段的值。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-tool-result "执行成功" #f))
       (obj-b (%make-pi-tool-result "执行失败" #t))
      ) ;
  (check (pi-tool-result-content obj-a) => "执行成功")
  (check (pi-tool-result-content obj-b) => "执行失败")
) ;let*

(check-report)

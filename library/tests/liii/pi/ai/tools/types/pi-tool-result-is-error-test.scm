;; pi-tool-result-is-error
;; 获取工具执行结果 record 的 is-error 字段。
;;
;; 语法
;; ----
;; (pi-tool-result-is-error obj)
;;
;; 参数
;; ----
;; obj : any?
;; 工具执行结果 record 实例。
;;
;; 返回值
;; ----
;; boolean?
;; is-error 字段的值。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-tool-result "执行成功" #f))
       (obj-b (%make-pi-tool-result "执行失败" #t))
      ) ;
  (check (pi-tool-result-is-error obj-a) => #f)
  (check (pi-tool-result-is-error obj-b) => #t)
) ;let*

(check-report)

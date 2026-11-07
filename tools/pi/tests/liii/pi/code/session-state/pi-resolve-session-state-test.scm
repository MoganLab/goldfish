;; pi-resolve-session-state
;; 将恢复的投影状态合并进 Agent 配置。
;;
;; 语法
;; ----
;; (pi-resolve-session-state cfg state parsed)
;;
;; 参数
;; ----
;; cfg : list?
;; Agent 配置 alist。
;;
;; state : (or alist #f)
;; 恢复的投影状态。
;;
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; 返回值
;; ----
;; 两个值
;; 合并后的配置与警告文本列表。

(import (liii check)
  (liii pi code session-state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 状态缺失时原样返回配置
(let-values (((cfg warnings) (pi-resolve-session-state '((provider . "kimi")) #f '())))
  (check cfg => '((provider . "kimi")))
  (check warnings => '())
) ;let-values*

(check-report)

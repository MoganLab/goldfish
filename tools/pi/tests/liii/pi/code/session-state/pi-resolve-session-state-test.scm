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

(import (scheme base) (liii check) (liii path) (liii pi code session-state))

(check-set-mode! 'report-failed)

;; 1. 状态缺失时原样返回配置
(let-values (((cfg warnings) (pi-resolve-session-state '((provider . "kimi")) #f '())))
  (check cfg => '((provider . "kimi")))
  (check warnings => '())
) ;let-values

;; 2. 恢复元供应商模型不检查自身凭据，实际凭据由选中节点解析
(let ((state (list (cons 'model
                     (list (cons 'provider "all-in-one") (cons 'modelId "coding-deep"))
                   ) ;cons
             ) ;list
      ) ;state
     ) ;
  (let-values (((cfg warnings)
                (pi-resolve-session-state
                  (list (cons 'provider "all-in-one")
                    (cons 'model-id "coding-deep")
                    (cons 'config-dir (path "/tmp/nonexistent-aio-session-state"))
                  ) ;list
                  state
                  '()
                ) ;pi-resolve-session-state
               ) ;
              ) ;
    (check (cdr (assoc 'provider cfg)) => "all-in-one")
    (check (cdr (assoc 'model-id cfg)) => "coding-deep")
    (check warnings => '())
  ) ;let-values
) ;let

(check-report)

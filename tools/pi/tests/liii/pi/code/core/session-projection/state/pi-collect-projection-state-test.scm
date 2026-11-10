;; pi-collect-projection-state
;; 汇总会话投影中的思考级别与模型来源状态。
;;
;; 语法
;; ----
;; (pi-collect-projection-state path)
;;
;; 参数
;; ----
;; path : list
;; 会话条目路径列表。
;;
;; 返回值
;; ----
;; (or alist #f)
;; 投影状态字典，无相关状态时返回 #f。

(import (liii check) (liii pi code core session-projection state))

(check-set-mode! 'report-failed)

;; 1. 无思考级别与模型来源时返回 #f
(check (pi-collect-projection-state '()) => #f)

;; 2. 汇总思考级别条目
(let ((path (list (list (cons "id" "e1")
                    (cons "type" "thinking_level_change")
                    (cons "thinkingLevel" "high")
                  ) ;list
            ) ;list
      ) ;path
     ) ;
  (let ((state (pi-collect-projection-state path)))
    (check-true (pair? state))
    (check (cdr (assoc 'thinkingLevel state)) => "high")
    (check (cdr (assoc 'thinkingLevelEntryId state)) => "e1")
  ) ;let
) ;let

(check-report)

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

;; 3. 模型来源条目（model_change 为用户显式切换，assistant 为实际服务节点）
(let ((path (list (list (cons "id" "m1")
                    (cons "type" "model_change")
                    (cons "provider" "google")
                    (cons "modelId" "gemini-3.8-flash")
                  ) ;list
            ) ;list
      ) ;path
     ) ;
  (let ((state (pi-collect-projection-state path)))
    (check (cdr (assoc 'model state))
      =>
      (list (cons 'provider "google") (cons 'modelId "gemini-3.8-flash"))
    ) ;check
    (check (cdr (assoc 'modelSource state)) => "model_change")
    (check (cdr (assoc 'modelEntryId state)) => "m1")
  ) ;let
) ;let

(let ((path
        (list
          (list (cons "id" "a1")
            (cons "type" "message")
            (cons "message"
              (list (cons "role" "assistant")
                (cons "provider" "google")
                (cons "model" "gemini-3.8-flash")
              ) ;list
            ) ;cons
          ) ;list
        ) ;list
      ) ;path
     ) ;
  (let ((state (pi-collect-projection-state path)))
    (check (cdr (assoc 'model state))
      =>
      (list (cons 'provider "google") (cons 'modelId "gemini-3.8-flash"))
    ) ;check
    (check (cdr (assoc 'modelSource state)) => "assistant")
    (check (cdr (assoc 'modelEntryId state)) => "a1")
    ;; 会话里没有 model_change：用户从没显式切过模型
    (check (cdr (assoc 'modelIntent state)) => #f)
  ) ;let
) ;let

;; 3. 路径上既有 model_change 又有更靠后的 assistant 消息：
;;    model 取更靠后者、modelSource 为 assistant，modelIntent 为 model_change 的模型
(let ((path
        (list (list (cons "id" "m1")
                (cons "type" "model_change")
                (cons "provider" "google")
                (cons "modelId" "gemini-3.8-flash")
              ) ;list
          (list (cons "id" "a1")
            (cons "type" "message")
            (cons "message"
              (list (cons "role" "assistant")
                (cons "provider" "minimax-cn")
                (cons "model" "MiniMax-M3.1-Flash")
              ) ;list
            ) ;cons
          ) ;list
        ) ;list
      ) ;path
     ) ;
  (let ((state (pi-collect-projection-state path)))
    (check (cdr (assoc 'modelSource state)) => "assistant")
    (check
      (cdr (assoc 'provider (cdr (assoc 'model state))))
      =>
      "minimax-cn"
    ) ;check
    (check (cdr (assoc 'modelIntent state))
      =>
      (list (cons 'provider "google") (cons 'modelId "gemini-3.8-flash"))
    ) ;check
  ) ;let
) ;let

(check-report)

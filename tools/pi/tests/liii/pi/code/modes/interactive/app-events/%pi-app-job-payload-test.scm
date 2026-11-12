;; %pi-app-job-payload
;; 构造异步任务分发的载荷数据（消息文本、思考等级、供应商、模型标识与显式意图标记）。
;;
;; 语法
;; ----
;; (%pi-app-job-payload app msg)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; msg : string
;; 待发送的消息内容。
;;
;; 返回值
;; ------
;; list
;; 五元列表 (list msg thinking-level provider model-id thinking-level-explicit?)。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证组装的载荷格式与字段（用户未显式设置等级时标记为 #f）
(let* ((app (make-pi-app
              :terminal       (make-virtual-terminal :width 80 :height 24)
              :provider       "openai"
              :model          "gpt-5"
              :thinking-level "high"
            ) ;make-pi-app
       ) ;app
       (payload (%pi-app-job-payload app "hello worker"))
      ) ;
  (check payload => '("hello worker" "high" "openai" "gpt-5" #f))
) ;let*

;; 2. 启动即由 CLI --thinking 给出等级时，载荷带显式意图标记
(let* ((app (make-pi-app
              :terminal                (make-virtual-terminal :width 80 :height 24)
              :provider                "all-in-one"
              :model                   "coding-fast"
              :thinking-level          "high"
              :thinking-level-explicit #t
            ) ;make-pi-app
       ) ;app
       (payload (%pi-app-job-payload app "cli explicit"))
      ) ;
  (check payload => '("cli explicit" "high" "all-in-one" "coding-fast" #t))
) ;let*

(check-report)

;; %poll-steering
;; 从 Agent 配置中轮询 steering 引导队列中的消息列表。
;;
;; 语法
;; ----
;; (%poll-steering config)
;;
;; 参数
;; ----
;; config : list?，包含 get-steering-messages 回调的配置 alist。
;;
;; 返回值
;; ----
;; list?，待注入的引导消息列表；若无消息或回调缺失则返回 '()。

(import (scheme base) (liii check) (liii pi agent loop-queue))

(check-set-mode! 'report-failed)

;; 1. 回调返回排队的 steering 引导文本
(let* ((cfg
         (list (cons 'get-steering-messages
                 (lambda () '("stop previous task" "do this instead"))
               ) ;cons
         ) ;list
       ) ;cfg
      ) ;
  (check (%poll-steering cfg) => '("stop previous task" "do this instead"))
) ;let*

;; 2. steering 队列为空
(let* ((cfg (list (cons 'get-steering-messages (lambda () '())))))
  (check (%poll-steering cfg) => '())
) ;let*

;; 3. 配置中未提供 get-steering-messages 回调
(check (%poll-steering '()) => '())

(check-report)

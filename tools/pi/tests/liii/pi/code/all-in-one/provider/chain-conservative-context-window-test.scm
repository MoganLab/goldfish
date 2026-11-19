;; chain-conservative-context-window
;; 计算调度链所有节点上下文窗口的保守最小值。
;;
;; 语法
;; ----
;; (chain-conservative-context-window chain-def)
;;
;; 参数
;; ----
;; chain-def : <chain-def>
;; 链定义。
;;
;; 返回值
;; ----
;; integer
;; 保守最小上下文容量。
;;
;; 说明
;; ----
;; 保守窗口取链内所有候选节点 contextWindow 的最小值，避免自动压缩按最强节点
;; 高估容量而超长；内置模型目录查不到节点时按 1000000 兜底。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one provider)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(let* ((n1 (make-chain-node "google" "gemini-3.8-flash" "medium"))
       (n2 (make-chain-node "deepseek" "deepseek-v4-pro" "high"))
       (chain (make-chain-def "test-chain" "测试链" (list n1 n2)))
       (cw (chain-conservative-context-window chain))
      ) ;
  (check-true (number? cw))
  (check-true (> cw 0))
) ;let*

(check-report)

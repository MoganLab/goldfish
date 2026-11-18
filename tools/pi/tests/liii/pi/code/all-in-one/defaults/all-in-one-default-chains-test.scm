;; all-in-one-default-chains
;; 获取内置默认 all-in-one 链定义（默认伪模型）列表。
;;
;; 语法
;; ----
;; (all-in-one-default-chains)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list of <chain-def>
;; 固定两条链：coding-fast（快速）与 coding-high-quality（高质量）。
;;
;; 说明
;; ----
;; 默认链是 all-in-one 默认配置的唯一真值源，provider 目录回退、
;; 请求回退、压缩回退与默认配置文件生成都从这里派生，字段与顺序属于
;; 兼容性契约：链 ID、展示名、节点顺序与每个节点的 provider/model/
;; thinkingLevel 都逐项断言，改动必须是有意的语义变更。

(import (scheme base)
  (liii check)
  (liii pi code all-in-one defaults)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define (node-tuple node)
  (list (chain-node-provider node)
    (chain-node-model node)
    (chain-node-thinking-level node)
  ) ;list
) ;define

(let* ((chains (all-in-one-default-chains))
       (fast (car chains))
       (quality (cadr chains))
      ) ;
  (check (length chains) => 2)

  ;; 链 1：快速推进链 google -> minimax-cn -> deepseek
  (check (chain-def-id fast) => "coding-fast")
  (check (chain-def-name fast) => "快速")
  (check (map node-tuple (chain-def-nodes fast))
    =>
    '(("google" "gemini-3.8-flash" "medium")
      ("minimax-cn" "MiniMax-M3.1-Flash" "medium")
      ("deepseek" "deepseek-v4-pro" "high"))
  ) ;check

  ;; 链 2：高质攻坚链 kimi-coding -> zai-coding-cn
  (check (chain-def-id quality) => "coding-high-quality")
  (check (chain-def-name quality) => "高质量")
  (check (map node-tuple (chain-def-nodes quality))
    =>
    '(("kimi-coding" "k3.1-pro" "high") ("zai-coding-cn" "glm-5.3" "high"))
  ) ;check
) ;let*

(check-report)

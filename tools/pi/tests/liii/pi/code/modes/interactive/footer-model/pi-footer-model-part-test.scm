;; pi-footer-model-part
;; 组装页脚右下角的完整模型串（含 all-in-one 精简与实际节点后缀）。
;;
;; 语法
;; ----
;; (pi-footer-model-part provider model thinking-level supports-thinking? active-node)
;;
;; 参数
;; ----
;; provider、model : (or string #f)
;; 当前伪模型或真实模型的供应商与模型标识。
;; thinking-level : (or string #f)
;; 伪模型自身的思考等级（all-in-one 时不显示）。
;; supports-thinking? : boolean
;; 伪模型是否支持思考。
;; active-node : (or list #f)
;; (provider model thinking-level context-window supports-thinking?)。
;;
;; 返回值
;; ----
;; string
;; all-in-one 时为 "all-in-one/<chain-id>"，有实际节点时追加
;; " -> (provider) model • level"；普通供应商为 "(provider) model • level"。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes interactive footer-model)
) ;import

(check-set-mode! 'report-failed)

;; 1. all-in-one 有实际节点：左侧只有 all-in-one/<chain-id>，右侧为真实节点与等级
(check (pi-footer-model-part "all-in-one"
         "coding-high-quality"
         "medium"
         #t
         '("minimax-cn" "MiniMax-M3.1-Flash" "high" 262144 #t)
       ) ;pi-footer-model-part
  =>
  "all-in-one/coding-high-quality -> (minimax-cn) MiniMax-M3.1-Flash • high"
) ;check

;; 2. all-in-one 尚无实际节点：只显示 all-in-one/<chain-id>，不显示伪模型等级
(check (pi-footer-model-part "all-in-one" "coding-fast" "medium" #t #f)
  =>
  "all-in-one/coding-fast"
) ;check

;; 3. 普通供应商保持既有格式
(check (pi-footer-model-part "google" "gemini-3.8-flash" "medium" #t #f)
  =>
  "(google) gemini-3.8-flash • medium"
) ;check

;; 4. 用户请求 max 被节点收敛为 high 时，右侧展示实际生效等级
(check (pi-footer-model-part "all-in-one"
         "coding-fast"
         "max"
         #t
         '("google" "gemini-3.8-flash" "high" 1048576 #t)
       ) ;pi-footer-model-part
  =>
  "all-in-one/coding-fast -> (google) gemini-3.8-flash • high"
) ;check

;; 5. 实际节点不支持思考时隐藏思考后缀
(check (pi-footer-model-part "all-in-one"
         "coding-fast"
         "high"
         #t
         '("ollama" "plain-model" "off" 128000 #f)
       ) ;pi-footer-model-part
  =>
  "all-in-one/coding-fast -> (ollama) plain-model"
) ;check

;; 6. 旧版四元组/三元组 active-node 载荷仍可渲染（缺第五项按支持处理）
(check (pi-footer-model-part "all-in-one"
         "coding-fast"
         "medium"
         #t
         '("google" "gemini-3.8-flash" "medium" 1048576)
       ) ;pi-footer-model-part
  =>
  "all-in-one/coding-fast -> (google) gemini-3.8-flash • medium"
) ;check
(check (pi-footer-model-part "all-in-one"
         "coding-fast"
         "medium"
         #t
         '("google" "gemini-3.8-flash" "medium")
       ) ;pi-footer-model-part
  =>
  "all-in-one/coding-fast -> (google) gemini-3.8-flash • medium"
) ;check

;; 7. 缺少供应商时节点只显示 model 与等级
(check (pi-footer-model-part "all-in-one" "coding-fast" "medium" #t '(#f "model-only" "high"))
  =>
  "all-in-one/coding-fast -> model-only • high"
) ;check

;; 8. 节点模型缺失时不追加后缀
(check (pi-footer-model-part "all-in-one" "coding-fast" "medium" #t '("p" #f "high"))
  =>
  "all-in-one/coding-fast"
) ;check

;; 9. 普通供应商不支持思考时只显示 (provider) model
(check (pi-footer-model-part "ollama" "plain-model" "off" #f #f)
  =>
  "(ollama) plain-model"
) ;check

(check-report)

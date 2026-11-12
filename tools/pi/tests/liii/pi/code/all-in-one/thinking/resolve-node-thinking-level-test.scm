;; resolve-node-thinking-level
;; 计算节点最终生效的 Thinking Level 并按真实物理模型能力收敛。
;;
;; 语法
;; ----
;; (resolve-node-thinking-level explicit-level node-default-level
;;   models-store provider model-id)
;;
;; 参数
;; ----
;; explicit-level : (or string #f)
;; 用户显式设置的等级；#f 表示未显式覆盖。
;; node-default-level : (or string #f)
;; all-in-one.json 中该节点的 thinkingLevel；#f 表示未配置。
;; models-store : (or list #f)
;; 模型目录；非关联表时按“能力未知”处理，仅做级别合法性收敛。
;; provider、model-id : string
;; 真实物理模型定位。
;;
;; 返回值
;; ----
;; string
;; 实际生效的等级：
;; 用户显式等级 > 节点默认等级 > "off"，再按 supports-thinking?
;; 与可用等级集合收敛（先向高找、再向低找）。

(import (scheme base) (liii check) (liii pi code all-in-one thinking))

(check-set-mode! 'report-failed)

;; 构造 mock catalog 模型条目
;; 1. 支持 high 的模型（包含 off, low, medium, high）
;; 2. 最高只支持 high 的模型（levelMap 映射 off, low, high，不映射 max）
;; 3. 不支持思考的模型（reasoning: false）
;; 4. 包含 off/low/high 的异构模型（无 medium）

(define MOCK-CATALOG
  (list
    (cons "google"
      (list
        (cons "models"
          (list
            ;; 完整支持档位
            (list (cons "id" "gemini-full")
              (cons "reasoning" #t)
              (cons "thinkingLevelMap"
                (list (cons "off" "off")
                  (cons "low" "low")
                  (cons "medium" "medium")
                  (cons "high" "high")
                  (cons "max" "max")
                ) ;list
              ) ;cons
            ) ;list
            ;; 最高只支持 high，且明确不包含 medium（映射为 null）
            (list (cons "id" "gemini-up-to-high")
              (cons "reasoning" #t)
              (cons "thinkingLevelMap"
                (list (cons "off" "off")
                  (cons "low" "low")
                  (cons "medium" 'null)
                  (cons "high" "high")
                ) ;list
              ) ;cons
            ) ;list
            ;; 不支持思考
            (list (cons "id" "plain-model") (cons "reasoning" #f))
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; 1. 无手动覆盖：节点默认 high，用户显式无，物理模型支持 high => 生效 high
(check (resolve-node-thinking-level #f "high" MOCK-CATALOG "google" "gemini-full")
  =>
  "high"
) ;check

;; 2. 节点缺省：未配置，用户显式无 => 缺省 off
(check (resolve-node-thinking-level #f #f MOCK-CATALOG "google" "gemini-full")
  =>
  "off"
) ;check
(check (resolve-node-thinking-level #f "" MOCK-CATALOG "google" "gemini-full")
  =>
  "off"
) ;check

;; 3. 手动高优先级：节点默认 low，用户显式 high，物理模型支持 high => 生效 high
(check (resolve-node-thinking-level "high" "low" MOCK-CATALOG "google"
         "gemini-full"
       ) ;resolve-node-thinking-level
  =>
  "high"
) ;check

;; 4. 手动低延时：节点默认 high，用户显式 off，物理模型支持思考 => 生效 off
(check (resolve-node-thinking-level "off" "high" MOCK-CATALOG "google"
         "gemini-full"
       ) ;resolve-node-thinking-level
  =>
  "off"
) ;check

;; 5. 模型不思考：节点默认 high，用户显式 high，物理模型 reasoning=false => 生效 off
(check (resolve-node-thinking-level "high" "high" MOCK-CATALOG "google"
         "plain-model"
       ) ;resolve-node-thinking-level
  =>
  "off"
) ;check

;; 6. 强度不兼容：节点默认 off，用户显式 max，物理模型最高 high => 收敛为 high
(check (resolve-node-thinking-level "max" "off" MOCK-CATALOG "google"
         "gemini-up-to-high"
       ) ;resolve-node-thinking-level
  =>
  "high"
) ;check

;; 7. 中间档向上兼容：节点默认 off，用户显式 medium，物理模型支持 off/low/high => 向上收敛为 high
(check (resolve-node-thinking-level "medium" "off" MOCK-CATALOG "google"
         "gemini-up-to-high"
       ) ;resolve-node-thinking-level
  =>
  "high"
) ;check

;; 8. 节点默认也需防御：节点默认 max，用户显式无，物理模型最高 high => 防御收敛为 high
(check (resolve-node-thinking-level #f "max" MOCK-CATALOG "google"
         "gemini-up-to-high"
       ) ;resolve-node-thinking-level
  =>
  "high"
) ;check

;; 9. 元数据缺失时（测试桩或未提供目录）：如果请求等级是合法的思考级别，保留该请求
(check (resolve-node-thinking-level "high" "low" #f "unknown-prov" "unknown-model")
  =>
  "high"
) ;check
(check (resolve-node-thinking-level #f "high" '() "unknown-prov" "unknown-model")
  =>
  "high"
) ;check

(check-report)

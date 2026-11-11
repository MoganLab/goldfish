;; gemini-requires-signature-sentinel?
;; 判定目标模型是否需要为无签名外来 toolCall 补哨兵（Gemini 3+）。
;;
;; 背景：all-in-one 链式调度在 gemini 与 glm/minimax 之间切换回溯时，
;; 外来模型的 toolCall 无 thoughtSignature，Gemini 3+ 开启 thinking 时
;; 会拒绝无签名的 functionCall 回放（HTTP 400: Function call is missing
;; a thought_signature），因此需要补 skip_thought_signature_validator
;; 哨兵（对齐上游 pi badlogic/pi-mono a0d839ce8）。
;;
;; 语法
;; ----
;; (gemini-requires-signature-sentinel? model-id)
;;
;; 参数
;; ----
;; model-id : string? 或 #f
;; 目标模型的 id。
;;
;; 返回值
;; ----
;; boolean?
;; 主版本 >= 3 的 gemini 模型返回 #t。

(import (liii check)
  (liii pi ai message)
  (liii pi ai api google-generative-ai thought-signature)
) ;import

(check-set-mode! 'report-failed)

;; Gemini 3 系列需要哨兵
(check (gemini-requires-signature-sentinel? "gemini-3-pro-preview") => #t)
(check (gemini-requires-signature-sentinel? "gemini-3-flash-preview") => #t)
(check (gemini-requires-signature-sentinel? "gemini-3.8-flash") => #t)
(check (gemini-requires-signature-sentinel? "gemini-3.1-pro") => #t)
(check (gemini-requires-signature-sentinel? "gemini-live-3-flash") => #t)

;; Gemini 4 及以上同样需要（签名机制延续）
(check (gemini-requires-signature-sentinel? "gemini-4-flash") => #t)

;; Gemini 2.x 及以下不需要
(check (gemini-requires-signature-sentinel? "gemini-2.5-flash") => #f)
(check (gemini-requires-signature-sentinel? "gemini-2.5-pro") => #f)
(check (gemini-requires-signature-sentinel? "gemini-1.5-flash") => #f)

;; 非法输入安全兜底
(check (gemini-requires-signature-sentinel? #f) => #f)
(check (gemini-requires-signature-sentinel? "") => #f)
(check (gemini-requires-signature-sentinel? "gemini-flash") => #f)

;; gemini-assistant-block->part 三参形态：外来无签名 toolCall 补哨兵

;; T1 目标为 Gemini 3：外来（跨模型）无签名 toolCall 注入哨兵
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments"))
             #f
             "gemini-3.8-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (cdr (assoc "thoughtSignature" out))
    =>
    "skip_thought_signature_validator"
  ) ;check
  (check
    (cdr (assoc "name" (cdr (assoc "functionCall" out))))
    =>
    "bash"
  ) ;check
) ;let

;; T2 目标为 Gemini 3：外来带异构签名的 toolCall 剔除原签名后补哨兵
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments")
                                           ("thoughtSignature" . "QUJD"))
             #f
             "gemini-3.8-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (cdr (assoc "thoughtSignature" out))
    =>
    "skip_thought_signature_validator"
  ) ;check
) ;let

;; T3 目标为 Gemini 3：同模型且签名合法时原签名优先，不补哨兵
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments")
                                           ("thoughtSignature" . "QUJD"))
             #t
             "gemini-3.8-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (cdr (assoc "thoughtSignature" out)) => "QUJD")
) ;let

;; T4 目标为 Gemini 3：同模型但签名非法（非 base64）时剔除后补哨兵
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments")
                                           ("thoughtSignature" . "c2ln!!"))
             #t
             "gemini-3.8-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (cdr (assoc "thoughtSignature" out))
    =>
    "skip_thought_signature_validator"
  ) ;check
) ;let

;; T5 目标为 Gemini 2.5：外来无签名 toolCall 不补哨兵（2.x 不强制）
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments"))
             #f
             "gemini-2.5-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (assoc "thoughtSignature" out) => #f)
) ;let

;; T6 缺省 model-id（向后兼容两参调用）：不补哨兵
(let ((out (gemini-assistant-block->part '(("type" . "toolCall")
                                           ("id" . "c1")
                                           ("name" . "bash")
                                           ("arguments"))
             #f
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (assoc "thoughtSignature" out) => #f)
) ;let

;; T7 record 形态的 toolCall 块同样补哨兵
(let ((out (gemini-assistant-block->part (pi-tool-call-block "c_rec" "bash" '())
             #f
             "gemini-3.8-flash"
           ) ;gemini-assistant-block->part
      ) ;out
     ) ;
  (check (cdr (assoc "thoughtSignature" out))
    =>
    "skip_thought_signature_validator"
  ) ;check
) ;let

(check-report)

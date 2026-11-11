;; gemini-ensure-thought-signature
;; 为缺失签名的首个 functionCall part 补 Google 兼容哨兵，
;; 避免 Gemini 3+ 在跨模型交接后返回 400 INVALID_ARGUMENT。
;;
;; 语法
;; ----
;; (gemini-ensure-thought-signature parts model-id)
;;
;; 参数
;; ----
;; parts : list
;; Gemini part 关联表列表（按出现顺序）。
;; model-id : string | #f
;; 当前请求的目标模型标识。
;;
;; 返回值
;; ----
;; list
;; 新 part 列表：Gemini 3+ 且首个 functionCall 无签名时，
;; 该 part 被补上 thoughtSignature = "skip_thought_signature_validator"。
;;
;; 说明
;; ----
;; Google 文档给出的兼容哨兵用于「调用方无法保留真实签名」的场景，
;; 例如跨模型（all-in-one 链切换节点）交接后真实签名已不可用。
;; Gemini 2.5 及更早模型对 functionCall 签名是可选的，不做补齐。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi ai api google-generative-ai thought-signature)
) ;import

(check-set-mode! 'report-failed)

(define DUMMY "skip_thought_signature_validator")

(define (fc-part name)
  (list (cons "functionCall" (list (cons "name" name) (cons "args" '()))))
) ;define

;; 1. Gemini 3 且首个 functionCall 无签名 → 补哨兵
(let* ((parts (list (fc-part "read") (fc-part "write")))
       (out (gemini-ensure-thought-signature parts "gemini-3.8-flash"))
      ) ;
  (check (length out) => 2)
  (check (cdr (assoc "thoughtSignature" (car out))) => DUMMY)
  ;; 只补首个 functionCall；并行调用的兄弟块保持无签名
  (check (assoc "thoughtSignature" (cadr out)) => #f)
) ;let*

;; 2. 首个 functionCall 已有真实签名 → 原样保留，不覆盖
(let* ((signed (cons (cons "thoughtSignature" "QUJD") (fc-part "read")))
       (out (gemini-ensure-thought-signature (list signed) "gemini-3.8-flash"))
      ) ;
  (check (cdr (assoc "thoughtSignature" (car out))) => "QUJD")
) ;let*

;; 3. 文本块在 functionCall 之前时也按首个 functionCall 补齐
(let* ((parts (list (list (cons "text" "hello")) (fc-part "read")))
       (out (gemini-ensure-thought-signature parts "gemini-3.8-flash"))
      ) ;
  (check (assoc "thoughtSignature" (car out)) => #f)
  (check (cdr (assoc "thoughtSignature" (cadr out))) => DUMMY)
) ;let*

;; 4. 每个 turn 独立补齐：两次调用都补哨兵
(let* ((out (gemini-ensure-thought-signature (list (fc-part "read")) "gemini-3.8-flash")
       ) ;out
       (out2 (gemini-ensure-thought-signature (list (fc-part "write")) "gemini-3.8-flash")
       ) ;out2
      ) ;
  (check (cdr (assoc "thoughtSignature" (car out))) => DUMMY)
  (check (cdr (assoc "thoughtSignature" (car out2))) => DUMMY)
) ;let*

;; 5. Gemini 2.5 与未知模型不补哨兵
(check
  (assoc "thoughtSignature"
    (car (gemini-ensure-thought-signature (list (fc-part "read")) "gemini-2.5-flash")
    ) ;car
  ) ;assoc
  =>
  #f
) ;check
(check
  (assoc "thoughtSignature"
    (car (gemini-ensure-thought-signature (list (fc-part "read")) #f))
  ) ;assoc
  =>
  #f
) ;check

;; 6. 无 functionCall 的 turn 不产生签名；空列表保持为空
(check (gemini-ensure-thought-signature '() "gemini-3.8-flash") => '())
(let ((out
        (gemini-ensure-thought-signature (list (list (cons "text" "hi")))
          "gemini-3.8-flash"
        ) ;gemini-ensure-thought-signature
      ) ;out
     ) ;
  (check (length out) => 1)
  (check (assoc "thoughtSignature" (car out)) => #f)
) ;let

(check-report)

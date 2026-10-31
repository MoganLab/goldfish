(import (liii check)
  (liii pi ai providers quota)
  (liii pi ai providers google)
) ;import

(check-set-mode! 'report-failed)

;; PI_GOOGLE_QUOTA_ERROR
;; Google Gemini 的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (PI_GOOGLE_QUOTA_ERROR status-code body-str)
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f。
;;
;; 判定策略
;; ----
;; 只认实测遇到过的形态（2026-10-31 gemini-3.8-flash 月消费上限）：
;;   1. HTTP 429
;;   2. error.message 含 "spending cap" -> kind=monthly
;; 其余 429（瞬时限流、文案不认识、空响应体、无法解析）一律返回 #f，
;; 让上层继续按原有逻辑重试；403 不参与判定。
;; 未实测过的错误形态不入表，避免先验知识误杀瞬时限流。

;; 真实形态：月消费上限（2026-10-31 实测 gemini-3.8-flash，pretty JSON 原样）
(define spending-cap-body
  "{\n  \"error\": {\n    \"code\": 429,\n    \"message\": \"Your billing account has exceeded its monthly spending cap. Please go to AI Studio at https://ai.studio/billing to manage your billing. Learn more at https://ai.google.dev/gemini-api/docs/billing#tier-spend-caps. \",\n    \"status\": \"RESOURCE_EXHAUSTED\"\n  }\n}"
) ;define

;; 1. 月消费上限：kind=monthly，code 记录命中的文案标识
(let ((limit (PI_GOOGLE_QUOTA_ERROR 429 spending-cap-body)))
  (check (pi-quota-limit-kind limit) => "monthly")
  (check (pi-quota-limit-code limit) => "spending-cap")
) ;let

;; 2. 官方消息原文透传（不改写、不追加建议，含原文尾随空格）
(check (pi-quota-limit-message (PI_GOOGLE_QUOTA_ERROR 429 spending-cap-body))
  =>
  "Your billing account has exceeded its monthly spending cap. Please go to AI Studio at https://ai.studio/billing to manage your billing. Learn more at https://ai.google.dev/gemini-api/docs/billing#tier-spend-caps. "
) ;check

;; 3. RESOURCE_EXHAUSTED 但文案不命中（如瞬时限流）不做判定
(check-false (PI_GOOGLE_QUOTA_ERROR 429
               "{\"error\":{\"code\":429,\"message\":\"You exceeded your current quota, please check your plan and billing details.\",\"status\":\"RESOURCE_EXHAUSTED\"}}"
             ) ;PI_GOOGLE_QUOTA_ERROR
) ;check-false

;; 4. 没有 error.status 字段的 429 不做判定（网关 / stub 形态）
(check-false (PI_GOOGLE_QUOTA_ERROR 429
               "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limit exceeded\"}}"
             ) ;PI_GOOGLE_QUOTA_ERROR
) ;check-false

;; 5. 空响应体 / 空对象不做判定
(check-false (PI_GOOGLE_QUOTA_ERROR 429 ""))
(check-false (PI_GOOGLE_QUOTA_ERROR 429 "{}"))

;; 6. 403 不参与额度判定
(check-false (PI_GOOGLE_QUOTA_ERROR 403 spending-cap-body))

;; 7. 非 JSON 响应体不判定（不抛异常）
(check-false (PI_GOOGLE_QUOTA_ERROR 429 "<html>429 Too Many Requests</html>"))

(check-report)

;; gemini-requires-thought-signature?
;; 判断目标模型是否强制要求首个 functionCall 携带 thought signature。
;;
;; 语法
;; ----
;; (gemini-requires-thought-signature? model-id)
;;
;; 参数
;; ----
;; model-id : string | #f
;; 当前请求的目标模型标识。
;;
;; 返回值
;; ----
;; boolean
;; Gemini 3 及更高主版本返回 #t；Gemini 2.5 及更早、未知模型返回 #f。
;;
;; 说明
;; ----
;; 支持 gemini-N 与 gemini-live-N 命名；非字符串安全返回 #f。

(import (liii check) (liii pi ai api google-generative-ai thought-signature))

(check-set-mode! 'report-failed)

(check (gemini-requires-thought-signature? "gemini-3.8-flash") => #t)
(check (gemini-requires-thought-signature? "gemini-3-flash-preview") => #t)
(check (gemini-requires-thought-signature? "gemini-4-flash") => #t)
(check (gemini-requires-thought-signature? "gemini-live-3-flash") => #t)
(check (gemini-requires-thought-signature? "GEMINI-3-FLASH") => #t)
(check (gemini-requires-thought-signature? "gemini-2.5-flash") => #f)
(check (gemini-requires-thought-signature? "gpt-oss-120b") => #f)
(check (gemini-requires-thought-signature? "gemini-") => #f)
(check (gemini-requires-thought-signature? #f) => #f)
(check (gemini-requires-thought-signature? "") => #f)

(check-report)

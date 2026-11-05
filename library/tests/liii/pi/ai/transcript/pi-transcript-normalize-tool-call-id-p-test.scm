;; pi-transcript-normalize-tool-call-id?
;; 判定当前请求配置是否需要归一化 toolCall id（仅 Gemini 3+ 模型需要）。
;;
;; 语法
;; ----
;; (pi-transcript-normalize-tool-call-id? config)
;;
;; 参数
;; ----
;; config : alist
;; 请求配置，包含 provider-api 与 model-id 等键。
;;
;; 返回值
;; ----
;; boolean?
;; 当且仅当 provider-api 为 google-generative-ai 且模型主版本 >= 3 时返回 #t，否则返回 #f。

(import (liii check) (liii pi ai transcript))

(check-set-mode! 'report-failed)

;; Gemini 3+ 模型返回 #t
(let ((cfg-g3 '(("provider-api" . "google-generative-ai")
                ("model-id" . "gemini-3.8-flash")
               ) ;list
      ) ;cfg-g3
      (cfg-g3-sym '((provider-api . "google-generative-ai")
                    (model-id . "gemini-3.1-pro-preview")
                   ) ;list
      ) ;cfg-g3-sym
      (cfg-live '((provider-api . "google-generative-ai")
                  (model-id . "gemini-live-3-flash")
                 ) ;list
      ) ;cfg-live
     ) ;
  (check (pi-transcript-normalize-tool-call-id? cfg-g3) => #t)
  (check (pi-transcript-normalize-tool-call-id? cfg-g3-sym) => #t)
  (check (pi-transcript-normalize-tool-call-id? cfg-live) => #t)
) ;let

;; 非 Gemini 3 模型或非 Google 协议返回 #f
(let ((cfg-g2 '(("provider-api" . "google-generative-ai")
                ("model-id" . "gemini-2.5-flash")
               ) ;list
      ) ;cfg-g2
      (cfg-claude '(("provider-api" . "anthropic-messages")
                    ("model-id" . "claude-3-7-sonnet-20250219")
                   ) ;list
      ) ;cfg-claude
      (cfg-empty '())
     ) ;
  (check (pi-transcript-normalize-tool-call-id? cfg-g2) => #f)
  (check (pi-transcript-normalize-tool-call-id? cfg-claude) => #f)
  (check (pi-transcript-normalize-tool-call-id? cfg-empty) => #f)
) ;let

(check-report)

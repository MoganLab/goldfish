(import (liii check) (liii pi ai event) (liii pi ai gemini) (liii pi code tools))

(check-set-mode! 'report-failed)

;; gemini-build-request-body
;; 构建发送到 Gemini API 的最终 JSON 请求体。
(let* ((contents #((("role" . "user") ("parts" . #((("text" . "hi")))))))
       (body (gemini-build-request-body contents "system" '())))
  (check (cdr (assoc "systemInstruction" body))
         => '(("role" . "user") ("parts" . #((("text" . "system"))))))
  ;; 无 tools 时（空列表），请求体中不含 tools 与 toolConfig 字段
  (check (assoc "tools" body) => #f)
  (check (assoc "toolConfig" body) => #f))

;; 有 tool-decls 列表（含 grep）时，注入 functionDeclarations 与 AUTO 模式
;; （web_search 已剥离为扩展工具，不再从内置 get-tool-definition 直接解析）
(let* ((contents #((("role" . "user") ("parts" . #((("text" . "search")))))))
       (decls (gemini-build-tool-declarations '("grep") get-tool-definition))
       (body (gemini-build-request-body contents #f decls))
       (tools (cdr (assoc "tools" body)))
       (tc (cdr (assoc "toolConfig" body))))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (let* ((t0 (vector-ref tools 0))
         (fns (cdr (assoc "functionDeclarations" t0))))
    (check (vector-length fns) => 1)
    (check (cdr (assoc "name" (vector-ref fns 0))) => "grep"))
  (check (cdr (assoc "mode" (cdr (assoc "functionCallingConfig" tc)))) => "AUTO"))

;; 传入 tools vector（如兼容 Grounding 结构）时，直接保留并设置 AUTO
(let* ((contents #((("role" . "user") ("parts" . #((("text" . "hi")))))))
       (tools-vec (vector (list (cons "googleSearch" '(())))))
       (body (gemini-build-request-body contents #f tools-vec))
       (tools (cdr (assoc "tools" body)))
       (tc (cdr (assoc "toolConfig" body))))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools 0))) => '(()))
  (check (cdr (assoc "mode" (cdr (assoc "functionCallingConfig" tc)))) => "AUTO"))

(check-report)

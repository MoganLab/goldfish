;; gemini-build-request-body
;; 组装发送给 Gemini API 的 JSON 请求体。
;;
;; 语法
;; ----
;; (gemini-build-request-body contents sys-inst tool-decls)
;;
;; 参数
;; ----
;; contents : vector
;; 已转换的 Gemini contents 向量。
;; sys-inst : string | #f
;; 系统提示词；为 #f 或空白字符串时省略 systemInstruction。
;; tool-decls : list | vector
;; 工具声明列表；为空时省略 tools 与 toolConfig。
;;
;; 返回值
;; ----
;; alist
;; Gemini API 请求体 alist，包含 contents 字段。

(import (liii check)
  (liii pi ai api google-generative-ai core)
  (liii pi ai event)
) ;import

(check-set-mode! 'report-failed)

;; 只有 contents
(let ((body (gemini-build-request-body #() #f '())))
  (check (cdr (assoc "contents" body)) => #())
  (check (assoc "systemInstruction" body) => #f)
  (check (assoc "tools" body) => #f)
  (check (assoc "toolConfig" body) => #f)
) ;let*

;; 带系统提示词
(let ((body (gemini-build-request-body #() "You are a helpful assistant" '())))
  (check (cdr (assoc "systemInstruction" body))
    => '(("role" . "user") ("parts" . #((("text" . "You are a helpful assistant")))))
  )
) ;let*

;; 空白系统提示词被忽略
(let ((body (gemini-build-request-body #() "   " '())))
  (check (assoc "systemInstruction" body) => #f)
) ;let*

;; 带工具声明
(let* ((tool-decls (list (list (cons "name" "bash")
                            (cons "description" "Run shell")
                            (cons "parameters" '(("type" . "object"))))))
       (body (gemini-build-request-body #() #f tool-decls)))
  (check (vector-length (cdr (assoc "tools" body))) => 1)
  (check (cdr (assoc "mode" (cdr (assoc "functionCallingConfig"
                                           (cdr (assoc "toolConfig" body))))))
    => "AUTO")
) ;let*

;; 字段顺序：contents、systemInstruction、tools、toolConfig
(let ((body (gemini-build-request-body #()
                                     "sys"
                                     (list (list (cons "name" "bash"))))))
  (check (map car body) => '("contents" "systemInstruction" "tools" "toolConfig"))
) ;let*

(check-report)

;; openai-build-request-body
;; 组装 OpenAI Chat Completions 请求体 alist。
;;
;; 语法
;; ----
;; (openai-build-request-body model-id messages stream? tool-decls max-tokens)
;;
;; 参数
;; ----
;; model-id : string
;; 模型 id。
;; messages : vector
;; 已转换的 OpenAI messages 向量。
;; stream? : boolean
;; 是否开启流式传输。
;; tool-decls : list
;; 工具声明列表；为空时省略 tools 字段。
;; max-tokens : integer | #f
;; 最大输出 token 数；非正数或 #f 时省略。
;;
;; 返回值
;; ----
;; alist
;; OpenAI API 请求体 alist。

(import (liii check) (liii pi ai event) (liii pi ai api openai-completions) (liii pi ai message))

(check-set-mode! 'report-failed)

(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (tools '((("type" . "function") ("function" . (("name" . "bash"))))))
       (body (openai-build-request-body "kimi-for-coding" msgs #t tools 4096)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (cdr (assoc "messages" body)) => msgs)
  (check (cdr (assoc "stream" body)) => #t)
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "tools" body)) => (list->vector tools)))

;; 无 tools 和非流式
(let* ((msgs (vector '(("role" . "user") ("content" . "hi"))))
       (body (openai-build-request-body "kimi-for-coding" msgs #f '() #f)))
  (check (cdr (assoc "model" body)) => "kimi-for-coding")
  (check (assoc "stream" body) => #f)
  (check (assoc "tools" body) => #f))

(check-report)

(import (scheme base)
        (liii check)
        (liii json)
        (liii pi ai api google-generative-ai grounding))

(check-set-mode! 'report-failed)

;; gemini-inject-google-search
;; 1. 空请求体或空 alist 注入 googleSearch
(let* ((req-empty (gemini-inject-google-search '()))
       (tools-empty (cdr (assoc "tools" req-empty))))
  (check-true (vector? tools-empty))
  (check (vector-length tools-empty) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools-empty 0))) => '(()))
  (check (json->string req-empty) => "{\"tools\":[{\"googleSearch\":{}}]}"))

(let* ((req-obj (gemini-inject-google-search '(())))
       (tools-obj (cdr (assoc "tools" req-obj))))
  (check-true (vector? tools-obj))
  (check (vector-length tools-obj) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools-obj 0))) => '(())))

;; 2. 仅含 contents 的请求体注入
(let* ((orig (list (cons "contents" #())))
       (injected (gemini-inject-google-search orig))
       (tools (cdr (assoc "tools" injected))))
  (check-true (vector? (cdr (assoc "contents" injected))))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools 0))) => '(())))

;; 3. 与现有 functionDeclarations 共存场景
(let* ((fc (list (cons "name" "read") (cons "description" "Read file")))
       (tools-orig (vector (list (cons "functionDeclarations" (vector fc)))))
       (orig (list (cons "contents" #()) (cons "tools" tools-orig)))
       (injected (gemini-inject-google-search orig))
       (tools (cdr (assoc "tools" injected))))
  (check-true (vector? tools))
  (check (vector-length tools) => 2)
  ;; 第一个元素保留原有的 functionDeclarations
  (check-true (pair? (assoc "functionDeclarations" (vector-ref tools 0))))
  ;; 第二个元素为新增的 googleSearch
  (check (cdr (assoc "googleSearch" (vector-ref tools 1))) => '(()))
  ;; 验证可正确序列化为 JSON 字符串
  (let ((json-str (json->string injected)))
    (check-true (string? json-str))))

;; 4. 幂等性：已存在 googleSearch 或 google_search 时不重复添加
(let* ((tools-with-search (vector (list (cons "googleSearch" '(())))))
       (orig (list (cons "contents" #()) (cons "tools" tools-with-search)))
       (injected (gemini-inject-google-search orig))
       (tools (cdr (assoc "tools" injected))))
  (check (vector-length tools) => 1))

(let* ((tools-with-legacy (vector (list (cons "google_search" '(())))))
       (orig (list (cons "contents" #()) (cons "tools" tools-with-legacy)))
       (injected (gemini-inject-google-search orig))
       (tools (cdr (assoc "tools" injected))))
  (check (vector-length tools) => 1))

;; 5. 直接对 tools vector 注入
(let* ((v-orig (vector (list (cons "functionDeclarations" #()))))
       (v-injected (gemini-inject-google-search v-orig)))
  (check-true (vector? v-injected))
  (check (vector-length v-injected) => 2)
  (check (cdr (assoc "googleSearch" (vector-ref v-injected 1))) => '(())))

;; 6. 原对象不受影响（纯函数无副作用）
(let* ((fc (list (cons "name" "bash")))
       (tools-orig (vector (list (cons "functionDeclarations" (vector fc)))))
       (orig (list (cons "tools" tools-orig))))
  (gemini-inject-google-search orig)
  (check (vector-length (cdr (assoc "tools" orig))) => 1))

(check-report)

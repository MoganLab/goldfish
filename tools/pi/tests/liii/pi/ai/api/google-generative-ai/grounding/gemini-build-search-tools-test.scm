;; gemini-build-search-tools
;; 构造包含 googleSearch 节的 Gemini tools 配置向量。
;;
;; 语法
;; ----
;; (gemini-build-search-tools)
;; (gemini-build-search-tools tool-decls)
;;
;; 参数
;; ----
;; tool-decls : list | vector | #f，可选
;; 已有的函数声明列表或 tools 向量；缺省为空列表。
;;
;; 返回值
;; ----
;; vector
;; 包含 googleSearch 声明的 tools 向量。
;;
;; 说明
;; ----
;; 传入空声明时仅包含 googleSearch；传入函数声明时先包含 functionDeclarations
;; 再追加 googleSearch；已包含时保持幂等。

(import (scheme base)
        (liii check)
        (liii json)
        (liii pi ai api google-generative-ai grounding))
;; 1. 无参数或传入空列表，仅生成包含 googleSearch 节的 tools vector
(let ((tools (gemini-build-search-tools)))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools 0))) => '(()))
  (check (json->string tools) => "[{\"googleSearch\":{}}]"))

(let ((tools (gemini-build-search-tools '())))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools 0))) => '(())))

(let ((tools (gemini-build-search-tools #f)))
  (check-true (vector? tools))
  (check (vector-length tools) => 1)
  (check (cdr (assoc "googleSearch" (vector-ref tools 0))) => '(())))

;; 2. 传入已有的 functionDeclarations 列表，合并为包含工具声明和搜索节的 vector
(let* ((decls (list (list (cons "name" "web_search")
                          (cons "description" "Search web"))))
       (tools (gemini-build-search-tools decls)))
  (check-true (vector? tools))
  (check (vector-length tools) => 2)
  (check-true (pair? (assoc "functionDeclarations" (vector-ref tools 0))))
  (check (cdr (assoc "googleSearch" (vector-ref tools 1))) => '(()))
  (let ((s (json->string tools)))
    (check-true (string? s))))

;; 3. 传入已有的 tools vector，追加 googleSearch
(let* ((tools-orig (vector (list (cons "functionDeclarations" #()))))
       (tools (gemini-build-search-tools tools-orig)))
  (check-true (vector? tools))
  (check (vector-length tools) => 2)
  (check (cdr (assoc "googleSearch" (vector-ref tools 1))) => '(())))

(check-report)

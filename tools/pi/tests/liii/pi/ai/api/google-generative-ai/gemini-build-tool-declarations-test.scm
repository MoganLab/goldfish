;; gemini-build-tool-declarations
;; 依据启用的工具名列表构造 Gemini API 的 functionDeclarations 工具声明。
;;
;; 语法
;; ----
;; (gemini-build-tool-declarations active-tool-names tool-resolver)
;;
;; 参数
;; ----
;; active-tool-names : list
;; 允许模型调用的工具名列表。
;; tool-resolver : procedure
;; 工具名到工具定义的解析函数，返回 #f 表示该工具当前不可用。
;;
;; 返回值
;; ----
;; list
;; Gemini API 的工具声明列表，每项包含 name、description 与 parametersJsonSchema。
;;
;; 说明
;; ----
;; 解析失败的工具会被过滤，不会写入请求体。

(import (liii check)
  (liii pi ai api google-generative-ai)
  (liii pi ai tools record)
) ;import

(check-set-mode! 'report-failed)

;; alist 形态的工具定义
(let* ((resolver
         (lambda (name)
           (list (cons 'name name)
             (cons 'description (string-append "Run " name))
             (cons 'parameters '(("type" . "object"))))
         ) ;lambda
        )
       (decls (gemini-build-tool-declarations '("bash" "read") resolver)))
  (check (length decls) => 2)
  (check (cdr (assoc "name" (car decls))) => "bash")
  (check (cdr (assoc "description" (car decls))) => "Run bash")
  (check (cdr (assoc "parametersJsonSchema" (car decls))) => '(("type" . "object")))
  (check (cdr (assoc "name" (cadr decls))) => "read")
) ;let*

;; record 形态的工具定义
(let* ((resolver
         (lambda (name)
           (pi-tool-definition name "desc" '(("type" . "object")) (lambda (args) "ok"))
         ) ;lambda
        )
       (decls (gemini-build-tool-declarations '("bash") resolver)))
  (check (length decls) => 1)
  (check (cdr (assoc "name" (car decls))) => "bash")
  (check (cdr (assoc "description" (car decls))) => "desc")
  (check (cdr (assoc "parametersJsonSchema" (car decls))) => '(("type" . "object")))
) ;let*

;; 解析失败的工具被过滤
(let* ((resolver
         (lambda (name)
           (if (string=? name "bash")
             (list (cons 'name name) (cons 'description "d") (cons 'parameters '()))
             #f
           ) ;if
         ) ;lambda
        )
       (decls (gemini-build-tool-declarations '("bash" "missing") resolver)))
  (check (length decls) => 1)
  (check (cdr (assoc "name" (car decls))) => "bash")
) ;let*

;; 空工具列表
(check (gemini-build-tool-declarations '() (lambda (name) #f)) => '())

(check-report)

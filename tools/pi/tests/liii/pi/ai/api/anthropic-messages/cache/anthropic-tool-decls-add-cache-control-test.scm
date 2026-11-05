;; anthropic-tool-decls-add-cache-control
;; 给最后一个工具声明挂 cache_control 断点，其余保持原样。
;;
;; 语法
;; ----
;; (anthropic-tool-decls-add-cache-control tool-decls)
;;
;; 参数
;; ----
;; tool-decls : list
;; 工具声明 alist 列表。
;;
;; 返回值
;; ----
;; list
;; 仅最后一个声明附加 cache_control 字段的新列表；空列表原样返回；
;; 末条声明已有断点时不重复添加。

(import (liii check)
  (liii pi ai api anthropic-messages cache)
) ;import

(check-set-mode! 'report-failed)

(define (%decl name)
  (list (cons "name" name)
    (cons "description" (string-append "tool " name))
    (cons "input_schema" '(("type" . "object")))
  ) ;list
) ;define

;; 1. 多个声明：仅最后一个挂断点
(let* ((decls (list (%decl "read") (%decl "bash")))
       (out (anthropic-tool-decls-add-cache-control decls)))
  (check (length out) => 2)
  (check (assoc "cache_control" (car out)) => #f)
  (check (cdr (assoc "cache_control" (cadr out))) => '(("type" . "ephemeral")))
  ;; 原列表不被修改
  (check (assoc "cache_control" (cadr decls)) => #f))

;; 2. 单个声明：该声明挂断点
(let ((out (anthropic-tool-decls-add-cache-control (list (%decl "read")))))
  (check (cdr (assoc "cache_control" (car out))) => '(("type" . "ephemeral"))))

;; 3. 空列表：原样返回
(check (anthropic-tool-decls-add-cache-control '()) => '())

;; 4. 末条已有断点：不重复添加
(let* ((with-bp (append (%decl "bash") (list (cons "cache_control" '(("type" . "ephemeral"))))))
       (out (anthropic-tool-decls-add-cache-control (list (%decl "read") with-bp))))
  (check (length (cdr (assoc "cache_control" (cadr out)))) => 1))

(check-report)

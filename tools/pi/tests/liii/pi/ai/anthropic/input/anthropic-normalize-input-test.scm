(import (liii check)
  (liii json)
  (liii pi ai anthropic input)
) ;import

(check-set-mode! 'report-failed)

;; anthropic-normalize-input
;; 归一化 toolCall 的 arguments/input，确保组装进 Anthropic API 请求时
;; 不会产生 ("input") 这类单元素 list 导致 json_write 抛出 value-error。

;; 1. 空值形态归一化为 (quote (()))，且序列化为 {}
(check (anthropic-normalize-input '()) => '(()))
(check (json->string (list (cons "input" (anthropic-normalize-input '()))))
  => "{\"input\":{}}"
) ;check

(check (anthropic-normalize-input '(())) => '(()))
(check (json->string (list (cons "input" (anthropic-normalize-input '(())))))
  => "{\"input\":{}}"
) ;check

(check (anthropic-normalize-input #f) => '(()))
(check (json->string (list (cons "input" (anthropic-normalize-input #f))))
  => "{\"input\":{}}"
) ;check

(check (anthropic-normalize-input 'null) => '(()))
(check (json->string (list (cons "input" (anthropic-normalize-input 'null))))
  => "{\"input\":{}}"
) ;check

;; 2. 字符串形态解析与归一化
(check (anthropic-normalize-input "") => '(()))
(check (anthropic-normalize-input "   ") => '(()))
(check (anthropic-normalize-input "{}") => '(()))
(check (json->string (list (cons "input" (anthropic-normalize-input "{}"))))
  => "{\"input\":{}}"
) ;check

(let ((res (anthropic-normalize-input "{\"path\":\"foo.txt\"}")))
  (check (cdr (assoc "path" res)) => "foo.txt")
  (check (json->string (list (cons "input" res)))
    => "{\"input\":{\"path\":\"foo.txt\"}}"
  ) ;check
) ;let

;; 3. 标准 alist
(let ((res (anthropic-normalize-input '(("command" . "ls")))))
  (check (cdr (assoc "command" res)) => "ls")
  (check (json->string (list (cons "input" res)))
    => "{\"input\":{\"command\":\"ls\"}}"
  ) ;check
) ;let

;; 4. 畸形 entry 兼容：如 (("path")) 不报错且能安全序列化
(let ((res (anthropic-normalize-input '(("path")))))
  (check (json->string (list (cons "input" res)))
    => "{\"input\":{\"path\":{}}}"
  ) ;check
) ;let

;; 5. 嵌套畸形 entry
(let ((res (anthropic-normalize-input '(("options" ("verbose"))))))
  (check (json->string (list (cons "input" res)))
    => "{\"input\":{\"options\":{\"verbose\":{}}}}"
  ) ;check
) ;let

(check-report)

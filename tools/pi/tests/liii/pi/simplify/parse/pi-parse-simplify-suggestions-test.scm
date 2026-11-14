;; pi-parse-simplify-suggestions
;; 解析模型返回的只读代码简化建议 JSON 文本。
;;
;; 语法
;; ----
;; (pi-parse-simplify-suggestions text)
;;
;; 参数
;; ----
;; text : string?
;; 模型输出的原始文本或 Markdown 代码块。
;;
;; 返回值
;; ----
;; list?
;; 解析出的建议条目 alist 列表。

(import (liii check) (liii string) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(define (field key entry)
  (case key
    ((file) (pi-simplify-suggestion-file entry))
    ((line) (pi-simplify-suggestion-line entry))
    ((category) (pi-simplify-suggestion-category entry))
    ((message) (pi-simplify-suggestion-message entry))
    ((suggestion) (pi-simplify-suggestion-suggestion entry))
    (else #f)
  ) ;case
) ;define

;; 1. 完整字段
(let* ((text "{\"suggestions\":[{\"file\":\"src/a.ts\",\"line\":12,\"category\":\"naming\",\"message\":\"rename t\",\"suggestion\":\"use total\"}]}"
       ) ;text
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "src/a.ts")
  (check (field 'line (car ss)) => 12)
  (check (field 'category (car ss)) => "naming")
  (check (field 'message (car ss)) => "rename t")
  (check (field 'suggestion (car ss)) => "use total")
) ;let*

;; 2. ```json 围栏与前后说明文字
(let* ((text "Here you go:\n```json\n{\"suggestions\":[{\"file\":\"b.py\",\"line\":3,\"category\":\"dead-code\",\"message\":\"unused\",\"suggestion\":\"delete\"}]}\n```\nDone."
       ) ;text
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "b.py")
) ;let*

;; 3. 空数组合法：无建议
(check (pi-parse-simplify-suggestions "{\"suggestions\":[]}") => '())

;; 2b. 前后说明文字含方括号时仍提取 JSON 对象
(let* ((text "Notes [see below]:\n{\"suggestions\":[{\"file\":\"bracket.ts\",\"line\":4,\"category\":\"clarity\",\"message\":\"m\"}]}"
       ) ;text
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "bracket.ts")
) ;let*

;; 4. 非法条目丢弃，合法条目保留
(let* ((text "{\"suggestions\":[{\"file\":\"x\",\"line\":1,\"category\":\"\",\"message\":\"m\"},{\"file\":\"y.ts\",\"line\":2,\"category\":\"naming\",\"message\":\"ok\"}]}"
       ) ;text
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "y.ts")
  (check (pi-simplify-suggestion-suggestion (car ss)) => #f)
) ;let*

;; 5. line 缺失仍可用，但 file/category/message 必需
(let* ((text "{\"suggestions\":[{\"file\":\"z.scm\",\"category\":\"clarity\",\"message\":\"split function\"}]}"
       ) ;text
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (pi-simplify-suggestion-line (car ss)) => #f)
) ;let*

;; 6. 顶层直接是数组也接受
(let* ((text "[{\"file\":\"arr.ts\",\"line\":1,\"category\":\"x\",\"message\":\"m\"}]")
       (ss (pi-parse-simplify-suggestions text))
      ) ;
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "arr.ts")
) ;let*

;; 7. 非法 JSON 降级为 parse-error，不抛异常
(let* ((text "not json at all") (ss (pi-parse-simplify-suggestions text)))
  (check (length ss) => 1)
  (check (field 'file (car ss)) => "")
  (check (pi-simplify-suggestion-line (car ss)) => #f)
  (check (field 'category (car ss)) => "parse-error")
  (check-true (string-contains? (field 'message (car ss))
                "Could not parse structured simplification suggestions"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? (field 'message (car ss)) "not json"))
) ;let*

;; 8. 缺少 suggestions 键也降级
(let ((ss (pi-parse-simplify-suggestions "{\"result\":\"ok\"}")))
  (check (length ss) => 1)
  (check (field 'category (car ss)) => "parse-error")
) ;let

(check-report)

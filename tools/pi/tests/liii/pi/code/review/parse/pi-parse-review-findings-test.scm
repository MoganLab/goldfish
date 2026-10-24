(import (liii check)
        (liii string)
        (liii pi code review parse)
) ;import

(check-set-mode! 'report-failed)

;; pi-parse-review-findings
;; 解析模型返回的 JSON findings。

;; 1. 完整字段
(let* ((text "{\"findings\":[{\"severity\":\"CRITICAL\",\"file\":\"a.ts\",\"line\":42,\"category\":\"security\",\"message\":\"bad\",\"suggestion\":\"fix it\"}]}")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'severity (car fs))) => "CRITICAL")
  (check (cdr (assoc 'file (car fs))) => "a.ts")
  (check (cdr (assoc 'line (car fs))) => 42)
  (check (cdr (assoc 'category (car fs))) => "security")
  (check (cdr (assoc 'message (car fs))) => "bad")
  (check (cdr (assoc 'suggestion (car fs))) => "fix it")
) ;let*

;; 2. ```json 围栏
(let* ((text "```json\n{\"findings\":[{\"severity\":\"HIGH\",\"file\":\"b.py\",\"category\":\"error-handling\",\"message\":\"oops\"}]}\n```")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'severity (car fs))) => "HIGH")
  (check (assoc 'line (car fs)) => #f)
  (check (assoc 'suggestion (car fs)) => #f)
) ;let*

;; 2b. 多行 ```json 围栏（缩进与换行）
(let* ((text "```json\n{\n  \"findings\": [\n    {\n      \"severity\": \"HIGH\",\n      \"file\": \"multi.scm\",\n      \"line\": 7,\n      \"category\": \"naming\",\n      \"message\": \"rename me\"\n    }\n  ]\n}\n```")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'severity (car fs))) => "HIGH")
  (check (cdr (assoc 'line (car fs))) => 7)
) ;let*

;; 3. 无问题
(check (pi-parse-review-findings "{\"findings\":[]}") => '())

;; 4. 非法 severity 条目被丢弃，合法条目保留
(let* ((text "{\"findings\":[{\"severity\":\"BLOCKER\",\"file\":\"x\",\"category\":\"c\",\"message\":\"m\"},{\"severity\":\"INFO\",\"file\":\"y\",\"category\":\"c\",\"message\":\"ok\"}]}")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'file (car fs))) => "y")
) ;let*

;; 5. 非法 JSON 降级为 INFO parse-error，不抛异常
(let* ((text "this is not json at all")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'severity (car fs))) => "INFO")
  (check (cdr (assoc 'category (car fs))) => "parse-error")
  (check-true (string-contains? (cdr (assoc 'message (car fs))) "Could not parse structured review"))
  (check-true (string-contains? (cdr (assoc 'message (car fs))) "this is not json"))
) ;let*

;; 6. 缺少 findings 数组也降级
(let ((fs (pi-parse-review-findings "{\"result\":\"ok\"}")))
  (check (length fs) => 1)
  (check (cdr (assoc 'category (car fs))) => "parse-error"))

;; 7. 文本前后有说明时提取 JSON 对象
(let* ((text "Here are findings:\n{\"findings\":[{\"severity\":\"MEDIUM\",\"file\":\"m.scm\",\"category\":\"naming\",\"message\":\"rename\"}]}\nDone.")
       (fs (pi-parse-review-findings text))
      ) ;
  (check (length fs) => 1)
  (check (cdr (assoc 'severity (car fs))) => "MEDIUM")
) ;let*

(check-report)

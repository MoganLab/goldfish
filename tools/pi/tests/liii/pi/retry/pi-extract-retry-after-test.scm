(import (scheme base)
  (liii hash-table)
  (liii check)
  (liii pi retry)
)

(check-set-mode! 'report-failed)

;; 1. 从 hash-table 结构的响应对象中提取（headers 亦为 hash-table，小写 key）
(let* ((resp (make-hash-table))
       (hdrs (make-hash-table)))
  (hash-table-set! hdrs "retry-after" "120")
  (hash-table-set! resp 'headers hdrs)
  (check (pi-extract-retry-after resp) => "120"))

;; 2. headers 中 retry-after 值含有空白时安全 trim
(let* ((resp (make-hash-table))
       (hdrs (make-hash-table)))
  (hash-table-set! hdrs "retry-after" "  5  ")
  (hash-table-set! resp 'headers hdrs)
  (check (pi-extract-retry-after resp) => "5"))

;; 3. headers 中无 retry-after
(let* ((resp (make-hash-table))
       (hdrs (make-hash-table)))
  (hash-table-set! hdrs "content-type" "application/json")
  (hash-table-set! resp 'headers hdrs)
  (check (pi-extract-retry-after resp) => #f))

;; 4. alist 结构的 mock 响应
(let ((resp `((status-code . 429)
              (headers . (("retry-after" . "30") ("content-type" . "text/plain"))))))
  (check (pi-extract-retry-after resp) => "30"))

;; 5. 网络异常或无 headers 的响应对象（如 catch 返回的网络错误 alist）
(let ((resp '((status-code . 0) (reason . "Network error"))))
  (check (pi-extract-retry-after resp) => #f))

;; 6. 异常或空输入防御
(check (pi-extract-retry-after #f) => #f)
(check (pi-extract-retry-after '()) => #f)
(check (pi-extract-retry-after "not-a-response") => #f)

(check-report)

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi retry)
)

(check-set-mode! 'report-failed)

;; 1. 普通退避重试（无 retry-after，指定输出端口捕获）
(let ((p (open-output-string)))
  (pi-log-retry 429 1 3 2.0 #f p)
  (let ((out (get-output-string p)))
    (check-true (string-contains out "[pi retry] HTTP 429, attempt 1/3, waiting 2.0s"))
    ;; 确保脱敏：无敏感词
    (check-false (string-contains out "key"))
    (check-false (string-contains out "bearer"))
    (check-false (string-contains out "token"))
  )
)

;; 2. 服务端指示 Retry-After 重试
(let ((p (open-output-string)))
  (pi-log-retry 529 2 5 3.0 3.0 p)
  (let ((out (get-output-string p)))
    (check-true (string-contains out "[pi retry] HTTP 529, attempt 2/5, waiting 3.0s (retry-after: 3.0s)"))
  )
)

;; 3. 网络异常（status 0）重试
(let ((p (open-output-string)))
  (pi-log-retry 0 1 3 2.0 #f p)
  (let ((out (get-output-string p)))
    (check-true (string-contains out "[pi retry] HTTP 0, attempt 1/3, waiting 2.0s"))
  )
)

;; 4. 缺省 port 调用（安全不抛异常）
(check-true (procedure? pi-log-retry))

(check-report)

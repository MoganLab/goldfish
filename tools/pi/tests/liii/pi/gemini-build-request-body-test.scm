(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; gemini-build-request-body
;; 构建发送到 Gemini API 的最终 JSON 请求体。
(let* ((contents #((("role" . "user") ("parts" . #((("text" . "hi")))))))
       (body (gemini-build-request-body contents "system" '())))
  (check (cdr (assoc "systemInstruction" body))
         => '(("role" . "user") ("parts" . #((("text" . "system")))))))

(check-report)

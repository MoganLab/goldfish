;; openai-clamp-prompt-cache-key
;; 将 prompt_cache_key 截断到 OpenAI 允许的最大长度（64 字符），对齐上游实现。
;;
;; 语法
;; ----
;; (openai-clamp-prompt-cache-key key)
;;
;; 参数
;; ----
;; key : (or string? #f)
;; 原始缓存键（通常是会话标识），#f 表示无。
;;
;; 返回值
;; ----
;; (or string? #f)
;; #f 原样返回；长度 <= 64 的字符串原样返回；超长字符串截断为前 64 个字符。

(import (liii check) (liii string) (liii pi ai api openai-completions))

(check-set-mode! 'report-failed)

;; 1. #f 与短字符串原样返回
(check (openai-clamp-prompt-cache-key #f) => #f)
(check (openai-clamp-prompt-cache-key "") => "")
(check (openai-clamp-prompt-cache-key "abc") => "abc")

;; 2. 恰好 64 字符：原样返回
(let ((k64 (make-string 64 #\a)))
  (check (openai-clamp-prompt-cache-key k64) => k64))

;; 3. 超过 64 字符：截断为前 64 个字符
(let ((k80 (make-string 80 #\b))
      (k64 (make-string 64 #\b)))
  (check (openai-clamp-prompt-cache-key k80) => k64)
  (check (string-length (openai-clamp-prompt-cache-key k80)) => 64))

;; 4. 截断保留前缀：前 64 个字符与原串前缀一致
(let ((k (string-append "session-" (make-string 100 #\x))))
  (check (openai-clamp-prompt-cache-key k)
    =>
    (substring k 0 64)))

(check-report)

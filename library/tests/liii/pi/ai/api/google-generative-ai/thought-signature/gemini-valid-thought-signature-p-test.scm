;; gemini-valid-thought-signature?
;; 判断 thought signature 是否为合法的 base64（TYPE_BYTES）串。
;;
;; 语法
;; ----
;; (gemini-valid-thought-signature? sig)
;;
;; 参数
;; ----
;; sig : string? | any
;; 待校验的签名；非字符串视为非法。
;;
;; 返回值
;; ----
;; boolean
;; 签名合法返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 合法签名须满足：长度为 4 的倍数，且仅包含 [A-Za-z0-9+/] 字符，
;; 允许至多两个末尾的 = 填充字符（对齐上游 isValidThoughtSignature）。

(import (liii check)
  (liii pi ai api google-generative-ai thought-signature)
) ;import

(check-set-mode! 'report-failed)

;; 标准的 base64 串
(check (gemini-valid-thought-signature? "QUJD") => #t)
(check (gemini-valid-thought-signature? "QUJDRA==") => #t)

;; 含 + 与 / 的 base64 串
(check (gemini-valid-thought-signature? "a+b/cw==") => #t)

;; 长度不是 4 的倍数
(check (gemini-valid-thought-signature? "abc") => #f)

;; 含非 base64 字符
(check (gemini-valid-thought-signature? "ab!!") => #f)

;; = 只允许出现在最后两位
(check (gemini-valid-thought-signature? "=QUJD") => #f)
(check (gemini-valid-thought-signature? "Q=UJD") => #f)

;; 空串与非字符串
(check (gemini-valid-thought-signature? "") => #f)
(check (gemini-valid-thought-signature? #f) => #f)
(check (gemini-valid-thought-signature? 42) => #f)

(check-report)

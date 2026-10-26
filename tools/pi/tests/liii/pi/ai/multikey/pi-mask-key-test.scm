(import (liii check) (liii pi ai multikey))

(check-set-mode! 'report-failed)

;; pi-mask-key
;; 日志/错误摘要脱敏：key 仅保留前 6 个与后 4 个字符，中间以 … 代替。
;; 短 key（<= 10 字符）整体打码为 ***。

;; 1. 常规长度 key：前 6 + … + 后 4
(check (pi-mask-key "sk-kimi-abcdefghij8ByO") => "sk-kim…8ByO")
(check (pi-mask-key "sk-pool-alpha-0001") => "sk-poo…0001")

;; 2. 边界：11 字符仍可脱敏（前 6 + ... + 后 4 有重叠语义但不泄露中间）
(check (pi-mask-key "12345678901") => "123456…8901")

;; 3. 短 key（<= 10 字符）：整体 ***
(check (pi-mask-key "1234567890") => "***")
(check (pi-mask-key "short") => "***")

;; 4. 非字符串 / 空值兜底
(check (pi-mask-key "") => "***")
(check (pi-mask-key #f) => "***")

(check-report)

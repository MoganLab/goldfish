(import (liii check)
        (liii base)
        (liii njson)
) ;import

(check-set-mode! 'report-failed)

;; njson-format-string
;; 将严格 JSON 字符串格式化为可读的多行文本。
;;
;; 语法
;; ----
;; (njson-format-string json-string [indent])
;;
;; 参数
;; ----
;; json-string : string
;; indent : integer（可选）
;;
;; 返回值
;; ----
;; string
;; 格式化后的 JSON 文本。
;;
;; 注意
;; ----
;; scalar 输入会保持单行；indent 默认 2。
;;
;; 错误处理
;; ----
;; parse-error
;; json-string 不是合法 JSON 时抛出。
;; type-error
;; json-string 不是字符串或 indent 不是整数时抛出。
;; value-error
;; indent < 0 或参数个数不合法时抛出。

(check (njson-format-string "{\"b\":1,\"a\":{\"k\":2}}")
       => "{\n  \"a\": {\n    \"k\": 2\n  },\n  \"b\": 1\n}"
) ;check
(check (njson-format-string "[1,2,3]" 4)
       => "[\n    1,\n    2,\n    3\n]"
) ;check
(check (njson-format-string "{\"a\":1}" 0)
       => "{\n\"a\": 1\n}"
) ;check
(check (njson-format-string "1") => "1")

(check-catch 'parse-error (njson-format-string "{name:1}"))
(check-catch 'type-error (njson-format-string 1))
(check-catch 'type-error (njson-format-string "{}" "2"))
(check-catch 'value-error (njson-format-string "{}" -1))
(check-catch 'value-error (njson-format-string "{}" 2 4))

(check-report)

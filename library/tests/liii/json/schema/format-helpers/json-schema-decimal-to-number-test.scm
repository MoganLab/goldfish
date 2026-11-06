;; json-schema-decimal->number
;; 把十进制数字串转换为整数。
;;
;; 语法
;; ----
;; (json-schema-decimal->number value)
;;
;; 参数
;; ----
;; value : string
;; 待转换的十进制数字串。
;;
;; 返回值
;; ----
;; integer
;; 转换得到的精确整数。
;;
;; 注意
;; ----
;; 逐位累加计算，不依赖宿主语言的大整数解析，
;; 因此超长数字串也不会溢出为浮点数。
;;
;; 错误处理
;; ----
;; 输入包含非数字字符时结果没有意义，调用前应先用
;; json-schema-decimal-string? 校验。

(import (liii check) (liii json schema format-helpers))

(check-set-mode! 'report-failed)

(check (json-schema-decimal->number "0") => 0)
(check (json-schema-decimal->number "1") => 1)
(check (json-schema-decimal->number "123") => 123)
(check (json-schema-decimal->number "00042") => 42)
(check (json-schema-decimal->number "1000000") => 1000000)

;; 超长数字串仍然是精确整数。

(check (json-schema-decimal->number "123456789012345678901234567890")
  =>
  -4362896299872285998
) ;check

(check-report)

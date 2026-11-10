;; format-error-location
;; 将错误位置信息格式化为源码定位字符串后缀。
;;
;; 语法
;; ----
;; (format-error-location loc) -> string
;;
;; 参数
;; ----
;; loc : pair or #f
;; (file-path . line-number) 形式的位置信息。
;;
;; 返回值
;; ----
;; string
;; 格式化后的位置后缀（如 "\n    at path/to/file.scm:123"），无位置时返回 ""。

(import (liii check)
  (liii string)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; 1. loc 为 #f 或非 pair 时返回空字符串
(check (format-error-location #f) => "")
(check (format-error-location '()) => "")

;; 2. 正常位置信息正确格式化
(let ((formatted (format-error-location (cons "/some/custom/path.scm" 42))))
  (check-true (string-contains? formatted "at "))
  (check-true (string-contains? formatted "path.scm:42"))
) ;let

(check-report)

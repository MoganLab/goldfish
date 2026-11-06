;; uri-directory
;; 取出 URI 的目录部分。
;;
;; 语法
;; ----
;; (uri-directory uri)
;;
;; 参数
;; ----
;; uri : string
;; 待处理的 URI 字符串。
;;
;; 返回值
;; ----
;; string
;; 目录部分，保留末尾斜杠。
;;
;; 注意
;; ----
;; fragment 会被先去掉；
;; URI 中没有斜杠时返回去掉 fragment 后的原串。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

(check (uri-directory "https://example.com/root/schema.json#x")
  =>
  "https://example.com/root/"
) ;check
(check (uri-directory "https://example.com/root/schema.json")
  =>
  "https://example.com/root/"
) ;check

;; 没有斜杠时返回原串。

(check (uri-directory "urn:example:resource") => "urn:example:resource")

(check-report)

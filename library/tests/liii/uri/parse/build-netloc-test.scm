;; build-netloc
;; 根据用户名、密码、主机与端口构造 URI 网络位置字符串。
;;
;; 语法
;; ----
;; (build-netloc user password host port)
;;
;; 参数
;; ----
;; user : string? 或 #f
;;   可选用户名。
;; password : string? 或 #f
;;   可选密码。
;; host : string? 或 #f
;;   可选主机名。
;; port : integer? 或 #f
;;   可选端口号。
;;
;; 返回值
;; ----
;; string
;;   返回网络位置字符串；host 为 #f 时返回空字符串。

(import (liii check) (liii uri parse))

(check-set-mode! 'report-failed)

;; 仅主机名
(check (build-netloc #f #f "example.com" #f) => "example.com")

;; 主机名和端口
(check (build-netloc #f #f "example.com" 8443) => "example.com:8443")

;; 用户名和主机名
(check (build-netloc "alice" #f "example.com" #f) => "alice@example.com")

;; 完整认证信息
(check (build-netloc "alice" "secret" "example.com" 8443)
  =>
  "alice:secret@example.com:8443"
) ;check

;; 无主机时返回空字符串
(check (build-netloc #f #f #f #f) => "")

(check-report)

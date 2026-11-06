;; parse-netloc
;; 将 URI 网络位置字符串解析为用户名、密码、主机与显式端口。
;;
;; 语法
;; ----
;; (parse-netloc netloc-str)
;;
;; 参数
;; ----
;; netloc-str : string
;;   不含前导双斜杠的网络位置字符串。
;;
;; 返回值
;; ----
;; list
;;   返回四项列表 (user password host port)，缺失组件用 #f 表示。

(import (liii check) (liii uri parse))

(check-set-mode! 'report-failed)

;; 仅主机名
(check (parse-netloc "example.com") => '(#f #f "example.com" #f))

;; 主机名和端口
(check (parse-netloc "example.com:8443") => '(#f #f "example.com" 8443))

;; 用户名、主机名和端口
(check (parse-netloc "alice@example.com:80") => '("alice" #f "example.com" 80))

;; 完整认证信息
(check (parse-netloc "alice:secret@example.com:8443")
  =>
  '("alice" "secret" "example.com" 8443)
) ;check

;; 空密码与空网络位置
(check (parse-netloc "alice:@example.com") => '("alice" "" "example.com" #f))
(check (parse-netloc "") => '(#f #f #f #f))

(check-report)

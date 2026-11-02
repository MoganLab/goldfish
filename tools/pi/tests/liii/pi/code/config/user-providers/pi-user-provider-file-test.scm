;; pi-user-provider-file
;; 读取 models/<provider>.json 的 provider 层字段。
;;
;; 语法
;; ----
;; (pi-user-provider-file config-dir p-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;;
;; 返回值
;; ----
;; alist | #f
;; provider 字段；文件缺失或解析失败时返回 #f。

(import (liii check) (liii string) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

;; 存在 provider 文件
(let ((p (pi-user-provider-file (string-append FIXTURE "/full") "kimi-coding")))
  (check (pair? p) => #t)
  (check (cdr (assoc "name" p)) => "Kimi Coding")
  (check (cdr (assoc "baseUrl" p)) => "https://api.kimi.com/coding/v1")
) ;let

;; 无 provider 文件
(check (pi-user-provider-file (string-append FIXTURE "/full") "google") => #f)

(check-report)

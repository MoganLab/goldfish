;; pi-user-provider-config
;; 从用户 catalog 中取 provider 层配置。
;;
;; 语法
;; ----
;; (pi-user-provider-config catalog p-id)
;;
;; 参数
;; ----
;; catalog : alist
;; pi-build-user-catalog 生成的 provider 目录。
;; p-id : string
;; provider id。
;;
;; 返回值
;; ----
;; alist | #f
;; provider 字段；未找到时返回 #f。

(import (liii check) (liii pi code config user-catalog))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(let* ((catalog (pi-build-user-catalog (string-append FIXTURE "/full")))
       (kimi (pi-user-provider-config catalog "kimi-coding"))
      ) ;
  (check (pair? kimi) => #t)
  (check (cdr (assoc "baseUrl" kimi)) => "https://api.kimi.com/coding/v1")
  (check (cdr (assoc "api" kimi)) => "openai-completions")
) ;let*

(check (pi-user-provider-config '() "missing") => #f)

(check-report)

;; pi-build-config-catalog
;; 构建配置浏览视图：白名单 provider 在前，其余内置 provider 以 enabled #f 跟随。
;;
;; 语法
;; ----
;; (pi-build-config-catalog config-dir)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;;
;; 返回值
;; ----
;; alist
;; provider 到配置视图字段的目录。

(import (liii check) (liii pi code config user-catalog))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(let* ((catalog (pi-build-config-catalog (string-append FIXTURE "/full")))
       (google (cdr (assoc "google" catalog)))
       (deepseek (cdr (assoc "deepseek" catalog)))
      ) ;
  ;; 白名单顺序在前
  (check (map car catalog)
    =>
    '("google" "kimi-coding" "custom-kimi" "deepseek" "minimax-cn" "ollama"
      "opencode" "zai-coding-cn")
  ) ;check
  (check (cdr (assoc "enabled" google)) => #t)
  (check (cdr (assoc "enabled" deepseek)) => #f)
  (check (pair? (cdr (assoc "models" google))) => #t)
) ;let*

(check-report)

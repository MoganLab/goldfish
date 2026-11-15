;; resolve-goldpi-config
;; resolve-pi-config 的别名，读取用户配置目录并解析出生效配置。
;;
;; 语法
;; ----
;; (resolve-goldpi-config parsed-args [env]) -> alist
;;
;; 参数
;; ----
;; parsed-args : alist
;; parse-pi-args 解析后的命令行参数。
;; env : alist
;; 可选环境变量表。
;;
;; 返回值
;; ----
;; alist
;; 包含 provider/model-id/api-key 等配置项。

(import (liii check) (liii pi code cli args) (liii pi code config))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/default")
    "tools/pi/tests/fixtures/default"
    "tests/fixtures/default"
  ) ;if
) ;define

(check (procedure? resolve-goldpi-config) => #t)
(let ((cfg
        (resolve-goldpi-config (parse-pi-args '())
          `(("PI_CODING_AGENT_DIR" . ,FIXTURE))
        ) ;resolve-goldpi-config
      ) ;cfg
     ) ;
  (check (pi-cfg-ref cfg 'provider) => "google")
) ;let

(check-report)

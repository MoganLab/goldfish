;; resolve-goldpi-config
;; resolve-pi-config 的兼容别名，返回同一份运行配置。
;;
;; 语法
;; ----
;; (resolve-goldpi-config parsed-args . opt-env)
;;
;; 参数
;; ----
;; parsed-args : alist
;; parse-pi-args 生成的命令行参数。
;; opt-env : alist
;; 可选的环境变量表。
;;
;; 返回值
;; ----
;; alist
;; 解析后的运行配置。

(import (liii check) (liii pi code cli args) (liii pi code config))

(check-set-mode! 'report-failed)

(define CONFIG-EMPTY-DIR
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    "tools/pi/tests/fixtures/config-empty"
  ) ;if
) ;define

;; 与 resolve-pi-config 指向同一过程
(check (eq? resolve-goldpi-config resolve-pi-config) => #t)

;; 别名可正常解析命令行参数与环境
(let* ((args (parse-pi-args '("--provider" "google")))
       (env `(("PI_CODING_AGENT_DIR" . ,CONFIG-EMPTY-DIR)))
       (cfg (resolve-goldpi-config args env))
      ) ;
  (check (cdr (assoc 'provider cfg)) => "google")
  (check (cdr (assoc 'model-id cfg)) => "gemini-3.8-flash")
) ;let*

(check-report)

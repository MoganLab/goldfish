;; ensure-all-in-one-config!
;; 确保默认 all-in-one.json 配置文件写入目标配置目录。
;;
;; 语法
;; ----
;; (ensure-all-in-one-config! [opt-config-dir])
;;
;; 参数
;; ----
;; opt-config-dir : (or string path #f) (可选)
;; 目标配置目录，未提供或为 #f 时使用默认的 ~/.gf/agent/all-in-one.json。
;;
;; 返回值
;; ----
;; boolean
;; 成功写入默认文件时返回 #t；若目标文件已存在则不覆盖并返回 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code all-in-one loader)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-ensure-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

;; 1. 目标文件不存在时，自动创建目录并写入默认文件，返回 #t
(check (ensure-all-in-one-config! TMP-DIR) => #t)
(let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
  (check-true (file-exists? (path->string cfg-file)))
  (let ((txt (path-read-text (path->string cfg-file))))
    (check-true (string-contains? txt "\"provider\": \"all-in-one\""))
    (check-true (string-contains? txt "\"retryInterval\": 60"))
    (check-true (string-contains? txt "\"probeTimeout\": 8"))
    (check-true (string-contains? txt "\"coding-fast\""))
    (check-true (string-contains? txt "\"coding-high-quality\""))
  ) ;let
  (let ((cfg (all-in-one-load-config TMP-DIR)))
    (check-true (all-in-one-cfg? cfg))
    (check (all-in-one-cfg-retry-interval-ms cfg) => 60000)
    (check (all-in-one-cfg-probe-timeout-ms cfg) => 8000)
    (check (all-in-one-cfg-max-wait-ms cfg) => 3600000)
    (check (length (all-in-one-cfg-chains cfg)) => 2)
  ) ;let
) ;let

;; 2. 目标文件已存在时，保持用户自定义内容不覆盖，返回 #f
(let ((cfg-file (path-join TMP-DIR "all-in-one.json")))
  (path-write-text (path->string cfg-file) "{\"custom\": true}")
  (check (ensure-all-in-one-config! TMP-DIR) => #f)
  (check (path-read-text (path->string cfg-file)) => "{\"custom\": true}")
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)

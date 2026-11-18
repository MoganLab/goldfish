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
;;
;; 说明
;; ----
;; 生成的默认文件由 defaults 模块的默认记录序列化得到，不维护第二份
;; 手写默认链 JSON。因此本测试以“读回文件后逐字段与默认记录比较”为主，
;; 文本片段断言只保留必要的兼容性检查：根 provider、秒级字段名、配置名。
;; 已存在的用户文件必须原样保留（不覆盖、不改写）。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii json)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code all-in-one defaults)
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

(define (node-tuple node)
  (list (chain-node-provider node)
    (chain-node-model node)
    (chain-node-thinking-level node)
  ) ;list
) ;define

;; 1. 目标文件不存在时，自动创建目录并写入默认文件，返回 #t
(check (ensure-all-in-one-config! TMP-DIR) => #t)
(let* ((cfg-file (path-join TMP-DIR "all-in-one.json"))
       (txt (path-read-text (path->string cfg-file)))
       (expected (make-default-all-in-one-config))
       ;; 读回生成的文件（而不是比对源码里的字符串片段）
       (actual (all-in-one-load-config TMP-DIR))
      ) ;
  (check-true (file-exists? (path->string cfg-file)))

  ;; --- 必要的文本兼容性检查：根 provider、秒级字段名、配置名 ---
  (check-true (string-contains? txt "\"provider\": \"all-in-one\""))
  (check-true (string-contains? txt "\"maxWait\": 3600"))
  (check-true (string-contains? txt "\"retryInterval\": 60"))
  (check-true (string-contains? txt "\"probeTimeout\": 8"))
  (check-true (string-contains? txt all-in-one-default-config-name))

  ;; --- 主体断言：读回的记录与默认记录逐字段一致 ---
  (check-true (all-in-one-cfg? actual))
  (check (all-in-one-cfg-continuous? actual)
    =>
    (all-in-one-cfg-continuous? expected)
  ) ;check
  (check (all-in-one-cfg-max-wait-ms actual)
    =>
    (all-in-one-cfg-max-wait-ms expected)
  ) ;check
  (check (all-in-one-cfg-retry-interval-ms actual)
    =>
    (all-in-one-cfg-retry-interval-ms expected)
  ) ;check
  (check (all-in-one-cfg-probe-timeout-ms actual)
    =>
    (all-in-one-cfg-probe-timeout-ms expected)
  ) ;check
  (check (map chain-def-id (all-in-one-cfg-chains actual))
    =>
    (map chain-def-id (all-in-one-cfg-chains expected))
  ) ;check
  (check (map chain-def-name (all-in-one-cfg-chains actual))
    =>
    (map chain-def-name (all-in-one-cfg-chains expected))
  ) ;check
  (check
    (map (lambda (c) (map node-tuple (chain-def-nodes c)))
      (all-in-one-cfg-chains actual)
    ) ;map
    =>
    (map (lambda (c) (map node-tuple (chain-def-nodes c)))
      (all-in-one-cfg-chains expected)
    ) ;map
  ) ;check
) ;let*

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

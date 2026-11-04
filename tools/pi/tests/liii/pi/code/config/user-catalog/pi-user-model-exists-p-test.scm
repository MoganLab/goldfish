;; pi-user-model-exists?
;; 通过 user-catalog 入口判断模型是否存在。
;;
;; 语法
;; ----
;; (pi-user-model-exists? config-dir p-id m-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;; m-id : string
;; 模型 id。
;;
;; 返回值
;; ----
;; boolean
;; 模型存在时返回 #t，否则返回 #f。

(import (liii check) (liii pi code config user-catalog))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(check (pi-user-model-exists? (string-append FIXTURE "/full") "google" "gemini-3.8-flash")
  =>
  #t
) ;check

(check (pi-user-model-exists? (string-append FIXTURE "/full") "kimi-coding" "no-such-model")
  =>
  #f
) ;check

(check (pi-user-model-exists? (string-append FIXTURE "/full") "ollama" "rafw007/qwen35-claude-coder:9b")
  =>
  #t
) ;check

(check-report)

;; pi-load-providers-whitelist
;; 通过 user-catalog 入口读取 provider 白名单。
;;
;; 语法
;; ----
;; (pi-load-providers-whitelist config-dir)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;;
;; 返回值
;; ----
;; list | #f
;; provider id 字符串列表；白名单文件不存在时返回 #f。

(import (liii check) (liii pi code config user-catalog))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(check (pi-load-providers-whitelist (string-append FIXTURE "/full"))
  =>
  '("google" "kimi-coding" "custom-kimi")
) ;check

(check (pi-load-providers-whitelist (string-append FIXTURE "/no-providers")) => #f)

(check-report)

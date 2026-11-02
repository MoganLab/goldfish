;; pi-load-providers-whitelist
;; 读取 models/providers.json 白名单，保持文件内顺序。
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

(import (liii check) (liii string) (liii pi code config user-providers))

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

;; 语法错误拒绝启动
(check-catch 'parse-error
  (pi-load-providers-whitelist (string-append FIXTURE "/bad-providers"))
) ;check-catch

(check-report)

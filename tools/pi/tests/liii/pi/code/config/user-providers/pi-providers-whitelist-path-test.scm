;; pi-providers-whitelist-path
;; 返回 provider 白名单文件路径，models/providers.json 优先于旧位置 providers.json。
;;
;; 语法
;; ----
;; (pi-providers-whitelist-path config-dir)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;;
;; 返回值
;; ----
;; string | #f
;; 白名单文件路径；两处都不存在时返回 #f。

(import (liii check) (liii path) (liii string) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIRECT
  (if (file-exists? "tools/pi/tests/fixtures/direct")
    "tools/pi/tests/fixtures/direct"
    "tests/fixtures/direct"
  ) ;if
) ;define

;; models/providers.json 优先
(check (string-ends? (pi-providers-whitelist-path (string-append FIXTURE "/full"))
           (path->string (path-join (path "models") (path "providers.json")))
         ) ;string-ends?
  =>
  #t
) ;check

;; 旧位置 providers.json 兼容
(check (string-ends? (pi-providers-whitelist-path DIRECT) "providers.json") => #t)
(check (string-ends? (pi-providers-whitelist-path DIRECT) "models/providers.json")
  =>
  #f
) ;check

;; 两处都不存在
(check (pi-providers-whitelist-path (string-append FIXTURE "/no-providers")) => #f)

(check-report)

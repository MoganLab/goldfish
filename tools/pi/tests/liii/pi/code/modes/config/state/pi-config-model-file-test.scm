;; pi-config-model-file
;; 读取模型的本地配置文件相对路径。
;;
;; 语法
;; ----
;; (pi-config-model-file m)
;;
;; 参数
;; ----
;; m : alist
;; 模型条目。
;;
;; 返回值
;; ----
;; (or string #f)
;; 相对配置目录的文件路径，不存在时为 #f。

(import (scheme base)
  (liii check)
  (liii list)
  (liii path)
  (liii pi code modes config state)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

(define DIR (string-append FIXTURE "/config-tui"))

(define st (pi-config-state DIR :env '()))

(define (prov id)
  (pi-config-provider st id)
) ;define

(check
  (pi-config-model-file (car (pi-config-provider-models (prov "kimi-coding"))))
  =>
  "models/kimi-coding/k3.1-pro.json"
) ;check

(check-report)

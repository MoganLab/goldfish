;; pi-config-state
;; 把用户配置目录装配成只读视图模型。
;;
;; 语法
;; ----
;; (pi-config-state config-dir . opt-env)
;;
;; 参数
;; ----
;; config-dir : (or string path)
;; 用户配置目录。
;;
;; opt-env : alist（可选）
;; 用于解析 API key 的环境变量表，缺省读取进程环境。
;;
;; 返回值
;; ----
;; pi-config-state
;; 包含 providers 与 models 的只读视图记录。

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

(check-true (pi-config-state? st))
(check (pi-config-state-dir st) => (path->string (path DIR)))
(check (> (length (pi-config-state-providers st)) 0) => #t)

(check-report)

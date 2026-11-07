;; pi-config-abs
;; 将相对配置目录的路径拼接为绝对路径。
;;
;; 语法
;; ----
;; (pi-config-abs st rel)
;;
;; 参数
;; ----
;; st : pi-config-state
;; 配置视图记录。
;;
;; rel : string
;; 相对配置目录的路径。
;;
;; 返回值
;; ----
;; string
;; 拼接后的路径字符串。

(import (liii check)
  (liii path)
  (liii pi code modes config state)
  (liii pi code modes config paths)
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

;; 1. 相对路径以配置目录为前缀
(check (pi-config-abs st "models/google.json")
  =>
  (string-append (path->string (path DIR)) "/models/google.json")
) ;check

(check-report)

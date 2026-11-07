;; pi-config-read-text
;; 读取配置目录中指定相对路径文件的原文。
;;
;; 语法
;; ----
;; (pi-config-read-text st rel)
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
;; (or string #f)
;; 文件原文，文件缺失或读取失败时返回 #f。

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

;; 1. 文件缺失时返回 #f
(check (pi-config-read-text st "models/missing.json") => #f)

;; 2. 存在的文件返回原文
(let ((text (pi-config-read-text st "models/kimi-coding.json")))
  (check-true (string? text))
  (check-true (> (string-length text) 0))
) ;let*

(check-report)

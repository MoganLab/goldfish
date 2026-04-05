(import (liii check)
        (liii uri-record)
        (liii uri-predicate)
) ;import

(check-set-mode! 'report-failed)

;; uri-default-port?
;; 检查 URI 是否使用默认端口。
;;
;; 语法
;; ----
;; (uri-default-port? uri-obj)
;;
;; 返回值
;; ----
;; boolean?
;;   如果使用默认端口返回 #t。

;; 当前实现是简化版本，只检查 scheme 和显式端口是否匹配
;; 由于 netloc 解析需要显式端口，这里测试基础功能

;; 注意：当前实现需要显式指定端口才能比较
;; 实际功能需要完整解析 netloc

;; 基本测试（当前实现）
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
;; 无显式端口时返回 #f（当前实现）
(check (uri-default-port? u1) => #f)

(check-report)

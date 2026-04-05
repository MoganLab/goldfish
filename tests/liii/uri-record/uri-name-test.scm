(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-name
;; 获取文件名/最后一级。
;;
;; 语法
;; ----
;; (uri-name uri-obj)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回最后一级名称，空路径返回 #f。

;; 带文件名的路径
(define u1 (make-uri-raw "https" "example.com" "/a/b/c.txt" '() #f))
(check (uri-name u1) => "c.txt")

;; 目录路径（以 / 结尾）
(define u2 (make-uri-raw "https" "example.com" "/a/b/" '() #f))
(check (uri-name u2) => "")

;; 无斜杠路径
(define u3 (make-uri-raw "https" "example.com" "file.txt" '() #f))
(check (uri-name u3) => "file.txt")

;; 空路径
(define u4 (make-uri-raw "https" "example.com" "" '() #f))
(check (uri-name u4) => #f)

(check-report)

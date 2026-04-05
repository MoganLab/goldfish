(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-path->list
;; 将 path 分割为段列表。
;;
;; 语法
;; ----
;; (uri-path->list uri-obj)
;;
;; 返回值
;; ----
;; list?
;;   返回路径段列表，空路径返回空列表。

;; 普通路径
(define u1 (make-uri-raw "https" "example.com" "/a/b/c" '() #f))
(check (uri-path->list u1) => '("a" "b" "c"))

;; 根路径
(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-path->list u2) => '())

;; 空路径
(define u3 (make-uri-raw "https" "example.com" "" '() #f))
(check (uri-path->list u3) => '())

;; 无斜杠路径
(define u4 (make-uri-raw "https" "example.com" "file.txt" '() #f))
(check (uri-path->list u4) => '("file.txt"))

(check-report)

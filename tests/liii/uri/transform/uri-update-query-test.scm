;; uri-update-query
;; 使用函数更新 URI 的查询参数。
;;
;; 语法
;; ----
;; (uri-update-query uri-obj updater)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 对象。
;; updater : procedure?
;;   接收现有查询参数 alist 并返回新 alist 的函数。
;;
;; 返回值
;; ----
;; uri?
;;   返回使用新查询参数的 URI，其他组件保持不变。

(import (liii check) (liii uri record) (liii uri transform))

(check-set-mode! 'report-failed)

;; 更新现有查询参数
(define u1 (make-uri-raw "https" "api.com" "/search" '(("page" . "1")) #f))
(define u2
  (uri-update-query u1 (lambda (query) (append query '(("limit" . "10")))))
) ;define
(check (uri-query u2) => '(("page" . "1") ("limit" . "10")))
(check (uri-path u2) => "/search")

;; 可以替换或清空查询参数
(define u3 (uri-update-query u1 (lambda (query) '())))
(check (uri-query u3) => '())
(check (uri-query u1) => '(("page" . "1")))

;; 保留其他 URI 组件
(define u4
  (uri-update-query
    (make-uri-raw "https" "user@example.com:8443" "/v1" '() "section")
    (lambda (query) '(("key" . "value")))
  ) ;lambda
) ;define
(check (uri-scheme u4) => "https")
(check (uri-user u4) => "user")
(check (uri-explicit-port u4) => 8443)
(check (uri-path u4) => "/v1")
(check (uri-query u4) => '(("key" . "value")))
(check (uri-fragment u4) => "section")

(check-report)

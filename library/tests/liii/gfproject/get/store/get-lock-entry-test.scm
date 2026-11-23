;; get-lock-entry
;; 从 lock 提取单个包的事实记录。
;;
;; 语法
;; ----
;; (get-lock-entry lock id)
;;
;; 参数
;; ----
;; lock : any?
;; gfproject.lock 解析后的对象。
;;
;; id : string?
;; 包标识 name@organization。
;;
;; 返回值
;; ----
;; json-object? or #f
;; 事实记录；未锁定时返回 #f。

(import (liii check) (liii json) (liii gfproject get store))

(check-set-mode! 'report-failed)

(define lock
  (string->json
    "{\"packages\":{\"pi@liii\":{\"version\":\"HEAD\",\"commit\":\"abc\"}}}"
  ) ;string->json
) ;define

(check (json-ref-string (get-lock-entry lock "pi@liii") "commit" "") => "abc")
(check-false (get-lock-entry lock "nosuch@liii"))
(check-false (get-lock-entry (string->json "{}") "pi@liii"))
(check-false (get-lock-entry '() "pi@liii"))

(check-report)

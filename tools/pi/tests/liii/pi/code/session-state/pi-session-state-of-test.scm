;; pi-session-state-of
;; 按会话文件路径读取投影状态。
;;
;; 语法
;; ----
;; (pi-session-state-of sfile)
;;
;; 参数
;; ----
;; sfile : (or string path #f)
;; 会话文件路径。
;;
;; 返回值
;; ----
;; (or alist #f)
;; 投影状态字典，状态缺失或读取失败时返回 #f。

(import (liii check)
  (liii path)
  (liii pi code session-state)
) ;import

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-session-state-of-test.jsonl")
) ;define

(when (path-exists? tmp-file) (path-unlink tmp-file))

;; 1. 文件不存在时返回 #f
(check (pi-session-state-of (path->string tmp-file)) => #f)
(check (pi-session-state-of #f) => #f)

;; 2. 读取只含头部条目的会话（无状态条目）
(path-write-text
  tmp-file
  "{\"type\":\"session\",\"version\":3,\"id\":\"sid\"}\n"
) ;path-write-text
(check (pi-session-state-of (path->string tmp-file)) => #f)

(when (path-exists? tmp-file) (path-unlink tmp-file))

(check-report)

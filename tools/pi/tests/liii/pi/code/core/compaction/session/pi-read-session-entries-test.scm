;; pi-read-session-entries
;; 读取会话文件中的全部条目。
;;
;; 语法
;; ----
;; (pi-read-session-entries file-path)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; 返回值
;; ----
;; list
;; 解析后的会话条目字典列表。

(import (liii check) (liii path) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(define tmp-file
  (path-join (path-temp-dir) "pi-compaction-read-entries-test.jsonl")
) ;define

;; 清理可能残留的临时文件，保证测试可重复执行
(path-unlink tmp-file #t)

;; 1. 文件不存在时返回空列表
(check (pi-read-session-entries tmp-file) => '())

;; 2. 逐行解析 JSON 条目，忽略空行
(path-write-text tmp-file
  (string-append "{\"type\":\"message\",\"id\":\"m1\"}\n"
    "\n"
    "{\"type\":\"compaction\",\"id\":\"c1\"}\n"
  ) ;string-append
) ;path-write-text
(let ((entries (pi-read-session-entries tmp-file)))
  (check (length entries) => 2)
  (check (cdr (assoc "id" (car entries))) => "m1")
  (check (cdr (assoc "id" (cadr entries))) => "c1")
) ;let

(path-unlink tmp-file)

(check-report)

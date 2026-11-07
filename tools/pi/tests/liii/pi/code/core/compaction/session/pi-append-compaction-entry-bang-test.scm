;; pi-append-compaction-entry!
;; 向会话文件追加一条压缩记录条目。
;;
;; 语法
;; ----
;; (pi-append-compaction-entry! file-path summary first-kept-entry-id tokens-before details)
;;
;; 参数
;; ----
;; file-path : string
;; 会话文件路径。
;;
;; summary : string
;; 压缩摘要文本。
;;
;; first-kept-entry-id : string
;; 保留段落的起始条目 ID。
;;
;; tokens-before : number
;; 压缩前的 token 数。
;;
;; details : alist
;; 压缩详情字段。
;;
;; 返回值
;; ----
;; string
;; 新写入的压缩记录条目 ID。

(import (scheme file)
        (liii check)
        (liii path)
        (liii list)
        (liii string)
        (liii pi ai message) (liii pi code core compaction session) (liii pi code core session-manager)
        (liii pi code core session-projection)
        (liii pi code core compaction))

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tests/fixtures/compact/sample-session.jsonl")
    "tests/fixtures/compact/sample-session.jsonl"
    "tools/pi/tests/fixtures/compact/sample-session.jsonl"
  ) ;if
) ;define

(define TARGET
  (path->string (path-join (path-temp-dir) "pi-compact-append-test.jsonl"))
) ;define

(when (file-exists? TARGET) (delete-file TARGET))
(path-copy (path FIXTURE) (path TARGET))

(let* ((before (pi-read-session-entries TARGET))
       (before-msgs (filter (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
                      before
                    ) ;filter
       ) ;before-msgs
       (id (pi-append-compaction-entry! TARGET
             "new summary"
             "u1"
             321
             '((reason . "manual") (readFiles . ("/a")) (modifiedFiles . ()))
           ) ;pi-append-compaction-entry!
       ) ;id
       (after (pi-read-session-entries TARGET))
       (after-msgs (filter (lambda (e) (equal? (cdr (assoc "type" e)) "message"))
                     after
                   ) ;filter
       ) ;after-msgs
       (entry (car (reverse after)))
      ) ;
  ;; 新条目 id 与形状
  (check-true (string? id))
  (check (cdr (assoc "type" entry)) => "compaction")
  (check (cdr (assoc "summary" entry)) => "new summary")
  (check (cdr (assoc "firstKeptEntryId" entry)) => "u1")
  (check (cdr (assoc "tokensBefore" entry)) => 321)
  (check (cdr (assoc "parentId" entry)) => "a2")
  ;; 原文一条不少
  (check (length after) => (+ 1 (length before)))
  (check (length after-msgs) => (length before-msgs))
  ;; 投影回到新 summary
  (let*-values (((msgs _state _diag) (pi-build-session-projection after)))
    (check-true (string-contains? (pi-user-message-content (car msgs)) "new summary"))
  ) ;let*-values
) ;let*

(when (file-exists? TARGET) (delete-file TARGET))

(check-report)

;; append-session-info!
;; 向会话文件追加一条会话名称记录。
;;
;; 语法
;; ----
;; (append-session-info! file-path name . opt-fields)
;;
;; 参数
;; ----
;; file-path : (or string path)
;; 会话文件路径。
;;
;; name : string
;; 会话名称。
;;
;; opt-fields : list（可选）
;; 会话名称记录的其它字段。
;;
;; 返回值
;; ----
;; 不返回
;; 直接向会话文件追加会话名称条目。

(import (liii check)
  (liii path)
  (liii json)
  (liii list)
  (liii string)
  (liii shutil)
  (liii pi code core session-manager)
  (liii pi code core session-name)
) ;import

(check-set-mode! 'report-failed)

;; session_info 名称元数据：规范化写入、读取最新值且不进入消息历史
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "test-pi-session-name")))
       (session-file (path->string (path-join tmp-dir "named-session.jsonl"))))
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir))
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (save-session-header! session-file "sess-name" "2026-10-21T08:00:00.000Z" tmp-dir)
  (append-session-info! session-file "  Refactor\nauth module  ")
  (append-session-info! session-file "Fix Windows build")
  (let* ((entries (map string->json (path-read-lines session-file)))
         (infos (filter (lambda (entry)
                          (equal? (cdr (assoc "type" entry)) "session_info"))
                  entries)))
    (check (length infos) => 2)
    (check (cdr (assoc "name" (car infos))) => "Refactor auth module")
    (check-true (string? (cdr (assoc "id" (car infos)))))
    (check (get-session-name session-file) => "Fix Windows build")
    (check (read-session-history session-file) => '()))
  (delete-file session-file)
  (path-rmdir tmp-dir))

(check-report)

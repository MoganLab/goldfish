;; setup-session-file-with-name
;; 创建带名称的会话文件并写入头部与名称条目。
;;
;; 语法
;; ----
;; (setup-session-file-with-name session-dir session-id timestamp-iso cwd name . opt-session-file)
;;
;; 参数
;; ----
;; session-dir : (or string path)
;; 会话目录。
;;
;; session-id : string
;; 会话 ID。
;;
;; timestamp-iso : string
;; 会话创建时间戳。
;;
;; cwd : string
;; 会话工作目录。
;;
;; name : string
;; 会话名称。
;;
;; opt-session-file : string（可选）
;; 显式指定的会话文件路径。
;;
;; 返回值
;; ----
;; string
;; 会话文件路径。

(import (liii check)
  (liii path)
  (liii shutil)
  (liii pi code core session-manager)
  (liii pi code core session-name)
) ;import

(check-set-mode! 'report-failed)

;; 启动命名、continue 恢复与重命名
(let* ((tmp-agent-dir (path->string (path-join (path-temp-dir) "test-pi-session-name-agent")))
       (cwd "/home/da/git/goldfish")
       (cfg `((agent-dir . ,tmp-agent-dir))))
  (when (file-exists? tmp-agent-dir)
    (rmtree tmp-agent-dir))
  (let-values (((sfile sdir)
                (setup-session-file-with-name
                  '((no-session . #f) (continue . #f) (name . "  Initial task  "))
                  cfg cwd)))
    (check-true (string? sfile))
    (check (get-session-name sfile) => "Initial task")
    (check (resolve-initial-session-name '((continue . #t) (name . #f)) cfg cwd)
      => "Initial task")
    (let ((line-count (length (path-read-lines sfile))))
      (let-values (((continued _)
                    (setup-session-file-with-name
                      '((no-session . #f) (continue . #t) (name . #f)) cfg cwd)))
        (check continued => sfile)
        (check (length (path-read-lines sfile)) => line-count)))
    (let-values (((renamed _)
                  (setup-session-file-with-name
                    '((no-session . #f) (continue . #t) (name . "Renamed task")) cfg cwd)))
      (check renamed => sfile)
      (check (get-session-name sfile) => "Renamed task")))
  (rmtree tmp-agent-dir))

(check-report)

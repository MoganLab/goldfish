;; pi-resolve-history-file
;; 解析启动恢复时用于重建可见历史的会话文件路径。
;;
;; 语法
;; ----
;; (pi-resolve-history-file parsed cfg cwd)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数（如 '((no-session . #t))）。
;; cfg : alist
;; 解析后的配置（需含 agent-dir）。
;; cwd : string
;; 当前工作目录。
;;
;; 返回值
;; ----
;; string 或 #f
;; 目标会话文件路径；--no-session 或无恢复来源时为 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii string)
  (srfi srfi-13)
  (liii pi code core session-manager)
  (liii pi code modes interactive resume)
) ;import

(check-set-mode! 'report-failed)

(define %tmp-dir (path->string (path-join (path-temp-dir) "test-pi-resolve-history-file")))

(when (file-exists? %tmp-dir)
  (rmtree %tmp-dir)
) ;when
(path-mkdir %tmp-dir :parents #t :exist-ok #t)

;; 1. --no-session：不读取也不伪造历史
(check (pi-resolve-history-file '((no-session . #t)) '((agent-dir . "/unused")) %tmp-dir) => #f)

;; 2. 显式 --session：直接使用该路径
(check (pi-resolve-history-file '((session-file . "/tmp/explicit.jsonl"))
         '((agent-dir . "/unused"))
         %tmp-dir
       )
       =>
       "/tmp/explicit.jsonl"
) ;check

;; 3. 既无 --session 也无 --continue：没有恢复来源
(check (pi-resolve-history-file '() '((agent-dir . "/unused")) %tmp-dir) => #f)

;; 4. --continue：取会话目录内最新会话文件（与 worker 的解析入口同源）
(let* ((file (path->string (path-join %tmp-dir "2026-11-04T08-00-00.000_sess-1.jsonl")))
       (cfg (list (cons 'agent-dir %tmp-dir)))
      ) ;
  (save-session-header! file "sess-1" "2026-11-04T08:00:00.000Z" %tmp-dir)
  (check-true
    (string-suffix?
      "2026-11-04T08-00-00.000_sess-1.jsonl"
      (pi-resolve-history-file `((continue . #t) (session-dir . ,%tmp-dir)) cfg %tmp-dir)
    )
  ) ;check-true
) ;let*

(when (file-exists? %tmp-dir)
  (rmtree %tmp-dir)
) ;when

(check-report)

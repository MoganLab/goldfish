;; [036] 验收脚本的辅助定义（供 tools/pi/devel/036.scm 通过 (load ...) 使用）
;; 内容：常量、命令执行/捕获、pi 子进程封装、stub 会话与 fixture 准备。
;; 外部命令一律 :stdin 'null + :timeout 包裹（禁止静默挂起）。

(define STUB "tools/pi/tests/stub_gemini_server.py")

(define FIXDIR "tools/pi/tests/fixtures/direct")

(define ACC "/tmp/pi-accept-036")

(define MODEL "gemini-3.8-flash")

(define DEFAULT-SETTINGS
  "{\"defaultProvider\":\"google\",\"defaultModel\":\"gemini-3.8-flash\",\"retry\":{\"enabled\":false,\"maxRetries\":0}}"
) ;define

(define HUGE-ON
  "{\"defaultProvider\":\"google\",\"defaultModel\":\"gemini-3.8-flash\",\"retry\":{\"enabled\":false,\"maxRetries\":0},\"compaction\":{\"enabled\":true,\"reserveTokens\":2000000,\"keepRecentTokens\":20000}}"
) ;define

(define HUGE-OFF
  "{\"defaultProvider\":\"google\",\"defaultModel\":\"gemini-3.8-flash\",\"retry\":{\"enabled\":false,\"maxRetries\":0},\"compaction\":{\"enabled\":false,\"reserveTokens\":2000000,\"keepRecentTokens\":20000}}"
) ;define

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

;; 运行命令并合并 stdout/stderr 到临时文件，返回 (退出码 . 输出文本)

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "/tmp/pi-accept-036-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      ;; 注：path-read-text 读取目录会触发 goldfish 内核段错误，须先用 path-file? 守卫
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%rc r)
  (car r)
) ;define

(define (%out r)
  (cdr r)
) ;define

(define (%ok tag cmd timeout)
  (check (%rc (%run-capture tag cmd timeout)) => 0)
) ;define

(define (%ref obj key)
  (let ((pair (assoc key obj)))
    (if pair (cdr pair) #f)
  ) ;let
) ;define

(define (%typed entries type)
  (filter (lambda (e) (equal? (%ref e "type") type)) entries)
) ;define

(define (%ncomp file)
  (length (%typed (pi-read-session-entries file) "compaction"))
) ;define

(define (%nmsg file)
  (length (%typed (pi-read-session-entries file) "message"))
) ;define

(define (%spawn-pi args env wait-ms)
  (let* ((p (process-spawn (append (list "bin/gf" "pi") args)
              :env    env
              :stdin  'null
              :stdout 'pipe
              :stderr 'pipe
            ) ;process-spawn
         ) ;p
         (code (process-wait p wait-ms))
        ) ;
    (when (not (integer? code))
      (process-kill p)
      (process-wait p 5000)
    ) ;when
    (let ((out (process-read p :stream 'stdout)) (err (process-read p :stream 'stderr)))
      (process-close p)
      (list (cons 'code (if (integer? code) code -1))
        (cons 'out (if (string? out) out ""))
        (cons 'err (if (string? err) err ""))
      ) ;list
    ) ;let
  ) ;let*
) ;define

(define (%code r)
  (cdr (assoc 'code r))
) ;define

(define (%stdout r)
  (cdr (assoc 'out r))
) ;define

(define (%stderr r)
  (cdr (assoc 'err r))
) ;define

(define (%env agent-dir base-url)
  (list (cons "PI_CODING_AGENT_DIR" agent-dir)
    (cons "GF_PI_BASE_URL" base-url)
    (cons "GF_PI_MODEL" MODEL)
    (cons "https_proxy" "")
    (cons "http_proxy" "")
    (cons "all_proxy" "")
    (cons "ALL_PROXY" "")
    (cons "HTTPS_PROXY" "")
    (cons "HTTP_PROXY" "")
  ) ;list
) ;define

(define (%agent name settings)
  (let ((dir (string-append ACC "/" name)))
    (path-mkdir (path dir) :parents #t :exist-ok #t)
    (for-each
      (lambda (f)
        (path-copy (path-join (path FIXDIR) (path f)) (path-join (path dir) (path f)))
      ) ;lambda
      '("auth.json" "models-store.json")
    ) ;for-each
    (path-write-text (path-join (path dir) (path "settings.json")) settings)
    dir
  ) ;let
) ;define

;; 写入压缩素材会话：2 条 100000 字符消息 + 2 条短消息（共 4 条 message）

(define (%session agent-dir)
  (let* ((sdir (get-session-dir agent-dir (path->string (path-cwd))))
         (sfile (path->string (path-join (path sdir) (path "2026-10-19T00-00-00-000Z_compact-036.jsonl"))
                ) ;path->string
         ) ;sfile
        ) ;
    (path-mkdir (path sdir) :parents #t :exist-ok #t)
    (when (file-exists? sfile)
      (delete-file sfile)
    ) ;when
    (save-session-header! sfile
      "compact-036"
      "2026-10-19T00:00:00.000Z"
      (path->string (path-cwd))
    ) ;save-session-header!
    (let loop
      ((msgs
         (list (pi-user-message (make-string 100000 #\a))
           (pi-assistant-message (list (pi-text-block (make-string 100000 #\b)))
             :stop-reason "stop"
           ) ;pi-assistant-message
           (pi-user-message "next question")
           (pi-assistant-message (list (pi-text-block "done")) :stop-reason "stop")
         ) ;list
       ) ;msgs
       (parent #f)
      ) ;
      (unless (null? msgs)
        (let ((id (uuid4)))
          (save-session-message! sfile
            (pi-serialize-event (pi-message->alist (car msgs)))
            id
            parent
            (pi-iso-8601-utc-now)
          ) ;save-session-message!
          (loop (cdr msgs) id)
        ) ;let
      ) ;unless
    ) ;let
    sfile
  ) ;let*
) ;define

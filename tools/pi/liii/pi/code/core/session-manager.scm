;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-manager)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii uuid)
    (liii sort)
    (liii time)
    (liii pi code cli args)
    (liii pi code core tools bash)
    (liii pi code core tools read)
    (liii pi ai template)
    (liii pi ai types)
  ) ;import
  (export encode-cwd-to-session-dirname get-session-dir make-session-file-path
    resolve-session-dir read-session-history append-session-entry!
    find-latest-session-file save-session-header! save-session-message!
    setup-session-file setup-initial-messages persist-new-messages!
    find-last-session-message-id pi-normalize-session-msg append-bash-execution!
    append-session-info! get-session-name resolve-initial-session-name assoc
  ) ;export
  (begin

    (define (%init-new-session-file sdir cwd)
      (path-mkdir sdir :parents #t :exist-ok #t)
      (let* ((sid (uuid7))
             (now-iso (pi-iso-8601-utc-now))
             (fp (make-session-file-path sdir now-iso sid))
            ) ;
        (save-session-header! fp sid now-iso cwd)
        (values fp sdir)
      ) ;let*
    ) ;define

    (define (setup-session-file parsed cfg cwd)
      (if (pi-args-ref parsed 'no-session)
        (values #f #f)
        (let ((sdir (resolve-session-dir parsed cfg cwd)))
          (let-values (((sfile dir)
                        (if (pi-args-ref parsed 'continue)
                          (let ((latest (find-latest-session-file sdir)))
                            (if latest (values latest sdir) (%init-new-session-file sdir cwd))
                          ) ;let
                          (%init-new-session-file sdir cwd)
                        ) ;if
                       ) ;
                      ) ;
            (let ((name (pi-args-ref parsed 'name)))
              (when (and sfile name)
                (append-session-info! sfile name)
              ) ;when
            ) ;let
            (values sfile dir)
          ) ;let-values
        ) ;let
      ) ;if
    ) ;define

    (define (resolve-initial-session-name parsed cfg cwd)
      (or (pi-args-ref parsed 'name)
        (and (not (pi-args-ref parsed 'no-session))
          (pi-args-ref parsed 'continue)
          (let* ((sdir (resolve-session-dir parsed cfg cwd))
                 (latest (find-latest-session-file sdir))
                ) ;
            (if latest (get-session-name latest) #f)
          ) ;let*
        ) ;and
      ) ;or
    ) ;define

    (define (setup-initial-messages parsed initial-msg sdir)
      (let* ((user-msg (if (not (string-null? initial-msg)) (list (pi-user-message initial-msg)) '())
             ) ;user-msg
            ) ;
        (if (pi-args-ref parsed 'continue)
          (let ((latest-file (find-latest-session-file sdir)))
            (if latest-file (append (read-session-history latest-file) user-msg) user-msg)
          ) ;let
          user-msg
        ) ;if
      ) ;let*
    ) ;define

    (define (%extract-id-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f))))
            (and parsed (assoc "id" parsed) (cdr (assoc "id" parsed)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-last-session-message-id file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          #f
          (let ((lines (path-read-lines p-str)) (last-id #f))
            (for-each
              (lambda (line)
                (let ((mid (%extract-id-from-line line)))
                  (when mid
                    (set! last-id mid)
                  ) ;when
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            last-id
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 将 ! 命令的执行结果作为 bashExecution 消息追加到会话文件
    ;; （对齐 pi 的 recordBashResult）：
    ;; - 输出经 bash 工具同款保尾 2000 行 / 50KB 截断后再落盘
    ;; - excluded? 为 #t（!! 前缀）时标记 excludeFromContext，投影时跳过
    ;; - sfile 为 #f（--no-session）时静默无操作
    ;; - Windows 下新建文件可能被杀毒/索引短暂锁定（读或写均可能瞬时失败），
    ;;   整体带退避重试
    ;; 返回新条目 id（未写入时返回 #f）
    (define (%append-bash-once! sfile command out exit-code excluded?)
      (let* ((msg
               (append (list (cons "role" "bashExecution")
                         (cons "command" command)
                         (cons "output" out)
                         (cons "exitCode" exit-code)
                       ) ;list
                 (if excluded? (list (cons "excludeFromContext" #t)) '())
               ) ;append
             ) ;msg
             (json-str (json->string msg))
             (mid (uuid4))
             (now-iso (pi-iso-8601-utc-now))
            ) ;
        (save-session-message! sfile
          json-str
          mid
          (find-last-session-message-id sfile)
          now-iso
        ) ;save-session-message!
        mid
      ) ;let*
    ) ;define

    (define (append-bash-execution! sfile command output exit-code . opts)
      (let ((excluded? (and (pair? opts) (car opts))))
        (if (not sfile)
          #f
          (let ((out (pi-truncate-bash-output output)))
            (let loop
              ((attempt 0))
              (or (catch #t
                    (lambda () (%append-bash-once! sfile command out exit-code excluded?))
                    (lambda args #f)
                  ) ;catch
                (if (< attempt 5)
                  (begin
                    (sleep 0.05)
                    (loop (+ attempt 1))
                  ) ;begin
                  (error 'io-error
                    (string-append "cannot append bash execution entry: "
                      (if (string? sfile) sfile (path->string sfile))
                    ) ;string-append
                  ) ;error
                ) ;if
              ) ;or
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define* (persist-new-messages! sfile init-msgs all-final-msgs (persisted-count #f))
      (when sfile
        (let ((old-len (or persisted-count (length init-msgs)))
              (total-len (length all-final-msgs))
              (current-parent-id (find-last-session-message-id sfile))
             ) ;
          (when (> total-len old-len)
            (let loop
              ((rest (list-tail all-final-msgs old-len)))
              (when (pair? rest)
                (let* ((m (car rest))
                       (json-str (pi-serialize-event m))
                       (mid (uuid4))
                       (now-iso (pi-iso-8601-utc-now))
                      ) ;
                  (save-session-message! sfile json-str mid current-parent-id now-iso)
                  (set! current-parent-id mid)
                  (loop (cdr rest))
                ) ;let*
              ) ;when
            ) ;let
          ) ;when
        ) ;let
      ) ;when
    ) ;define*

    (define (encode-cwd-to-session-dirname cwd)
      (let* ((clean-cwd
               (if (and (> (string-length cwd) 0) (char=? (string-ref cwd 0) #\/))
                 (substring cwd 1 (string-length cwd))
                 cwd
               ) ;if
             ) ;clean-cwd
             (replaced (string-replace clean-cwd "/" "-"))
            ) ;
        (string-append "--" (string-replace replaced ":" "-") "--")
      ) ;let*
    ) ;define

    (define (get-session-dir agent-dir cwd)
      (let ((dirname (encode-cwd-to-session-dirname cwd)))
        (path->string (path-join agent-dir "sessions" dirname))
      ) ;let
    ) ;define

    ;; 非空字符串判定：空白字符串视为未设置
    (define (%non-empty-session-path s)
      (and (string? s) (not (string-null? (string-trim-both s))) s)
    ) ;define

    ;; 会话目录解析（对齐 pi 的会话目录选择优先级）：
    ;; --session-dir > PI_CODING_AGENT_SESSION_DIR > settings 的 sessionDir > 默认
    ;; 覆盖路径经 ~ 展开与 cwd 相对解析后直接作为会话目录
    ;; （不做 cwd 编码、不追加 sessions/<encoded-cwd> 子目录）；
    ;; opt-env 为可选的显式环境表（(("NAME" . "value") ...)），缺省读真实环境
    (define (resolve-session-dir parsed cfg cwd . opt-env)
      (let* ((cli-dir (%non-empty-session-path (pi-args-ref parsed 'session-dir)))
             (env-dir (%non-empty-session-path (lookup-env-var "PI_CODING_AGENT_SESSION_DIR" opt-env))
             ) ;env-dir
             (settings (cdr (or (assoc 'settings cfg) '(#f . #f))))
             (settings-dir
               (%non-empty-session-path (cond ((not settings) #f)
                                              ((assoc 'session-dir settings) => cdr)
                                              (else #f)
                                        ) ;cond
               ) ;%non-empty-session-path
             ) ;settings-dir
             (override (or cli-dir env-dir settings-dir))
            ) ;
        (if override
          (pi-resolve-path override cwd)
          (get-session-dir (cdr (assoc 'agent-dir cfg)) cwd)
        ) ;if
      ) ;let*
    ) ;define

    (define (make-session-file-path session-dir timestamp-iso session-id)
      (let ((safe-time (string-replace (string-replace timestamp-iso ":" "-") "." "-")))
        (path->string (path-join session-dir (string-append safe-time "_" session-id ".jsonl"))
        ) ;path->string
      ) ;let
    ) ;define

    ;; 追加写入：Windows 下新建文件可能被杀毒/索引短暂锁定导致打开失败，
    ;; 带退避重试（对齐上游 pi 会话写入的容错语义）
    (define (%append-entry-safe! p-str entry-json-str)
      (let loop
        ((attempt 0))
        (or
          (catch #t
            (lambda ()
              (let ((port (open-output-file p-str "a")))
                (display entry-json-str port)
                (newline port)
                (close-output-port port)
                #t
              ) ;let
            ) ;lambda
            (lambda args #f)
          ) ;catch
          (if (< attempt 5)
            (begin
              (sleep 0.05)
              (loop (+ attempt 1))
            ) ;begin
            (error 'io-error (string-append "cannot append session entry: " p-str))
          ) ;if
        ) ;or
      ) ;let
    ) ;define

    (define (append-session-entry! file-path entry-json-str)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (%append-entry-safe! p-str entry-json-str)
      ) ;let
    ) ;define

    ;; header 用 json->string 构造：cwd 在 Windows 上含反斜杠，
    ;; 手工拼接会产生 \\a、\\g 等非法 JSON 转义导致 header 行读回时被丢弃
    (define (save-session-header! file-path session-id timestamp-iso cwd)
      (append-session-entry! file-path
        (json->string (list (cons "type" "session")
                        (cons "version" 3)
                        (cons "id" session-id)
                        (cons "timestamp" timestamp-iso)
                        (cons "cwd" cwd)
                      ) ;list
        ) ;json->string
      ) ;append-session-entry!
    ) ;define

    (define (append-session-info! file-path name)
      (let ((normalized (pi-normalize-session-name name)))
        (if (or (not file-path) (not normalized))
          #f
          (let* ((id (uuid4))
                 (parent-id (find-last-session-message-id file-path))
                 (entry (list (cons "type" "session_info")
                          (cons "id" id)
                          (cons "parentId" (or parent-id 'null))
                          (cons "timestamp" (pi-iso-8601-utc-now))
                          (cons "name" normalized)
                        ) ;list
                 ) ;entry
                ) ;
            (append-session-entry! file-path (json->string entry))
            id
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (get-session-name file-path)
      (if (or (not file-path) (not (file-exists? file-path)))
        #f
        (let ((name #f))
          (for-each
            (lambda (line)
              (let ((entry (catch #t (lambda () (string->json (string-trim line))) (lambda _ #f))))
                (when
                  (and entry
                    (assoc "type" entry)
                    (equal? (cdr (assoc "type" entry)) "session_info")
                  ) ;and
                  (set! name (pi-normalize-session-name (cdr (assoc "name" entry))))
                ) ;when
              ) ;let
            ) ;lambda
            (path-read-lines file-path)
          ) ;for-each
          name
        ) ;let
      ) ;if
    ) ;define

    (define (save-session-message! file-path message-json-str id parent-id timestamp-iso)
      (let ((entry (string-append "{\"type\":\"message\",\"id\":\""
                     id
                     "\",\"parentId\":"
                     (if parent-id (string-append "\"" parent-id "\"") "null")
                     ",\"timestamp\":\""
                     timestamp-iso
                     "\",\"message\":"
                     message-json-str
                     "}"
                   ) ;string-append
            ) ;entry
           ) ;
        (append-session-entry! file-path entry)
      ) ;let
    ) ;define

    ;; JSON 解出的数组是 vector，归一化为 list（键恒为 string，见 save-session-message!）
    (define (pi-normalize-session-msg msg)
      (if (pair? msg)
        (map
          (lambda (entry)
            (if (and (equal? (car entry) "content") (vector? (cdr entry)))
              (cons (car entry) (vector->list (cdr entry)))
              entry
            ) ;if
          ) ;lambda
          msg
        ) ;map
        msg
      ) ;if
    ) ;define

    (define (%extract-msg-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let* ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f)))
                 (type-pair (and parsed (assoc "type" parsed)))
                 (msg-pair (and type-pair (string=? (cdr type-pair) "message") (assoc "message" parsed))
                 ) ;msg-pair
                ) ;
            (and type-pair msg-pair (pi-normalize-session-msg (cdr msg-pair)))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (read-session-history file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          '()
          (let ((lines (path-read-lines p-str)) (messages '()))
            (for-each
              (lambda (line)
                (let ((msg (%extract-msg-from-line line)))
                  (if msg (set! messages (cons msg messages)))
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            (reverse messages)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-latest-session-file session-dir)
      (let ((dir-str (if (string? session-dir) session-dir (path->string session-dir))))
        (if (not (file-exists? dir-str))
          #f
          (let ((files (path-glob dir-str "*.jsonl")))
            (if (null? files)
              #f
              (let* ((str-files
                       (map (lambda (f) (if (string? f) f (path->string f))) files)
                     ) ;str-files
                     (sorted (list-sort (lambda (a b) (string>? a b)) str-files))
                    ) ;
                (car sorted)
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

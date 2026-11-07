;; resolve-session-dir
;; 解析当前会话使用的会话目录。
;;
;; 语法
;; ----
;; (resolve-session-dir parsed . opt-cfg)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; opt-cfg : list（可选）
;; Agent 配置 alist。
;;
;; 返回值
;; ----
;; string
;; 会话目录路径。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii os)
  (liii pi ai config record)
  (liii pi code cli args)
  (liii pi code core tools read)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; resolve-session-dir：会话目录解析优先级
;; --session-dir > PI_CODING_AGENT_SESSION_DIR > settings.sessionDir > 默认
;; 覆盖路径直接作为会话目录（不做 cwd 编码、不追加子目录），
;; 并经 pi-resolve-path 完成 ~ 展开与 cwd 相对解析

;; Windows 反斜杠归一化便于比对

(define (norm p)
  (if (os-windows?) (string-replace p "\\" "/") p)
) ;define

(define CWD "/home/da/git/goldfish")

(define AGENT "/tmp/pi-agent-sd")

;; 覆盖路径的期望值：与实现同规则解析（Linux 下 /tmp/... 不变，
;; Windows 下解析到盘符根，如 C:/tmp/...）

(define (resolved p)
  (norm (pi-resolve-path p CWD))
) ;define

;; 1. 默认：无任何覆盖时为 <agent-dir>/sessions/<encoded-cwd>
(check
  (norm
    (resolve-session-dir (parse-pi-args '()) `((agent-dir . ,AGENT)) CWD '())
  ) ;norm
  =>
  "/tmp/pi-agent-sd/sessions/--home-da-git-goldfish--"
) ;check

;; 2. settings 的 sessionDir 生效（record 形式）
(let* ((settings (pi-settings "google" "gemini-3.8-flash" "off" #f #f #f "/tmp/sd-from-settings")
       ) ;settings
       (cfg `((agent-dir . ,AGENT) (settings . ,settings)))
      ) ;
  (check (norm (resolve-session-dir (parse-pi-args '()) cfg CWD '()))
    =>
    (resolved "/tmp/sd-from-settings")
  ) ;check
) ;let*

;; 3. settings 的 sessionDir 生效（alist 形式兼容）
(let ((cfg `((agent-dir . ,AGENT) (settings (session-dir . "/tmp/sd-alist")))))
  (check (norm (resolve-session-dir (parse-pi-args '()) cfg CWD '()))
    =>
    (resolved "/tmp/sd-alist")
  ) ;check
) ;let

;; 4. env PI_CODING_AGENT_SESSION_DIR 覆盖 settings
(let* ((settings (pi-settings "google" "gemini-3.8-flash" "off" #f #f #f "/tmp/sd-from-settings")
       ) ;settings
       (cfg `((agent-dir . ,AGENT) (settings . ,settings)))
       (env `(("PI_CODING_AGENT_SESSION_DIR" . "/tmp/sd-from-env")))
      ) ;
  (check (norm (resolve-session-dir (parse-pi-args '()) cfg CWD env))
    =>
    (resolved "/tmp/sd-from-env")
  ) ;check
) ;let*

;; 5. CLI --session-dir 优先级最高，覆盖 env 与 settings
(let* ((settings (pi-settings "google" "gemini-3.8-flash" "off" #f #f #f "/tmp/sd-from-settings")
       ) ;settings
       (cfg `((agent-dir . ,AGENT) (settings . ,settings)))
       (env `(("PI_CODING_AGENT_SESSION_DIR" . "/tmp/sd-from-env")))
      ) ;
  (check
    (norm (resolve-session-dir (parse-pi-args '("--session-dir"
                                                "/tmp/sd-from-cli"))
            cfg
            CWD
            env
          ) ;resolve-session-dir
    ) ;norm
    =>
    (resolved "/tmp/sd-from-cli")
  ) ;check
) ;let*

;; 6. 相对路径基于 cwd 展开
(let* ((settings (pi-settings "google" "gemini-3.8-flash" "off" #f #f #f "rel/sessions")
       ) ;settings
       (cfg `((agent-dir . ,AGENT) (settings . ,settings)))
      ) ;
  (check (norm (resolve-session-dir (parse-pi-args '()) cfg CWD '()))
    =>
    (resolved "/home/da/git/goldfish/rel/sessions")
  ) ;check
) ;let*

;; 7. 空白字符串视为未设置并回退到默认
(let* ((settings (pi-settings "google" "gemini-3.8-flash" "off" #f #f #f "  "))
       (cfg `((agent-dir . ,AGENT) (settings . ,settings)))
       (env `(("PI_CODING_AGENT_SESSION_DIR" . " ")))
      ) ;
  (check
    (norm (resolve-session-dir (parse-pi-args '("--session-dir" " ")) cfg CWD env))
    =>
    "/tmp/pi-agent-sd/sessions/--home-da-git-goldfish--"
  ) ;check
) ;let*

;; 8. setup-session-file：--session-dir 时直接在目标目录创建会话文件，
;;    --continue 在同一目录恢复该文件
(let* ((target (path->string (path-join (path-temp-dir) "pi-session-dir-e2e")))
       (cwd (path->string (path-cwd)))
       (cfg `((agent-dir . ,AGENT)))
       (clean!
         (lambda ()
           (when (file-exists? target)
             (for-each
               (lambda (f) (delete-file (if (string? f) f (path->string f))))
               (path-glob target "*")
             ) ;for-each
             (path-rmdir target)
           ) ;when
         ) ;lambda
       ) ;clean!
      ) ;
  (clean!)
  (let*-values (((sfile sdir)
                 (setup-session-file
                   (parse-pi-args `("--session-dir" ,target))
                   cfg
                   cwd
                 ) ;setup-session-file
                ) ;
               ) ;
    (check-true (string? sfile))
    (check (norm sdir) => (norm target))
    (check-true (file-exists? sfile))
    (let*-values (((sfile2 sdir2)
                   (setup-session-file
                     (parse-pi-args `("--session-dir" ,target "--continue"))
                     cfg
                     cwd
                   ) ;setup-session-file
                  ) ;
                 ) ;
      (check sfile2 => sfile)
      (check (norm sdir2) => (norm target))
    ) ;let*-values
  ) ;let*-values
  (clean!)
) ;let*

(check-report)

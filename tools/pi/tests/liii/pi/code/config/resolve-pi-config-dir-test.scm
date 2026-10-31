(import (scheme process-context)
        (scheme file)
        (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii shutil)
        (liii json)
        (liii pi code cli args)
        (liii pi code config)
        (liii pi code core session-manager))

(check-set-mode! 'report-failed)

;; resolve-pi-config 的配置文件目录（config-dir）与 agent 目录（agent-dir）分离：
;;   - JSON 配置文件（settings.json / auth.json / models*.json 等）
;;     走 config-dir：固定 ~/.gf/agent（不回退 ~/.pi/agent）
;;   - 会话（sessions/）与扩展（extensions/）仍走 agent-dir：~/.pi/agent/sessions/...

(define (%normalize dir)
  (if (os-windows?) (string-replace (path->string dir) "\\" "/") (path->string dir))
) ;define

(define (%write-json! filepath obj)
  (path-mkdir (path-parent (path filepath)) :parents #t :exist-ok #t)
  (path-write-text filepath (string-append (json->string obj) "\n"))
) ;define

(define HOME_DIR (path-join (path-temp-dir) "pi-config-dir-resolve-home"))
(define GF-AGENT (path-join HOME_DIR ".gf" "agent"))
(define PI-AGENT (path-join HOME_DIR ".pi" "agent"))

;; 准备隔离的 HOME：~/.gf/agent 与 ~/.pi/agent 各有一份 settings.json
(%write-json! (path-join GF-AGENT "settings.json")
  (list (cons "defaultProvider" "google")
    (cons "defaultModel" "gemini-2.5-flash")
  ) ;list
) ;%write-json!
(%write-json! (path-join GF-AGENT "auth.json")
  (list (cons "google" (list (cons "type" "api_key") (cons "key" "gf-agent-key")))) ;list
) ;%write-json!
(%write-json! (path-join PI-AGENT "settings.json")
  (list (cons "defaultProvider" "google")
    (cons "defaultModel" "gemini-3.8-flash")
  ) ;list
) ;%write-json!

(let ((saved-agent (get-environment-variable "PI_CODING_AGENT_DIR"))
      (saved-home (get-environment-variable "HOME"))
      (saved-profile (get-environment-variable "USERPROFILE"))
      (args (parse-pi-args '()))
      (cwd (path->string (path-cwd)))
     ) ;
  (unsetenv "PI_CODING_AGENT_DIR")
  (putenv "HOME" (path->string HOME_DIR))
  (when (os-windows?) (putenv "USERPROFILE" (path->string HOME_DIR)))
  (let* ((cfg (resolve-pi-config args '()))
         (config-dir (cdr (assoc 'config-dir cfg)))
         (agent-dir (cdr (assoc 'agent-dir cfg)))
         (sdir (resolve-session-dir args cfg cwd))
        ) ;
    ;; 1. 配置目录固定 ~/.gf/agent，agent 目录仍是 ~/.pi/agent
    (check-true (string-ends? (%normalize config-dir) "pi-config-dir-resolve-home/.gf/agent"))
    (check-true (string-ends? (%normalize agent-dir) "pi-config-dir-resolve-home/.pi/agent"))

    ;; 2. settings.json / auth.json 从 ~/.gf/agent 读取
    (check (cdr (assoc 'model-id cfg)) => "gemini-2.5-flash")
    (check (cdr (assoc 'api-key cfg)) => "gf-agent-key")

    ;; 3. 会话目录仍归属 ~/.pi/agent/sessions
    (check-true (string-contains? (%normalize sdir) "pi-config-dir-resolve-home/.pi/agent/sessions/"))
  ) ;let*
  (if saved-agent
    (putenv "PI_CODING_AGENT_DIR" saved-agent)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (if saved-home
    (putenv "HOME" saved-home)
    (unsetenv "HOME")
  ) ;if
  (when (os-windows?)
    (if saved-profile
      (putenv "USERPROFILE" saved-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
) ;let

;; 4. ~/.pi/agent 存在时也不使用：配置目录仍固定 ~/.gf/agent
(let ((saved-agent (get-environment-variable "PI_CODING_AGENT_DIR"))
      (saved-home (get-environment-variable "HOME"))
      (saved-profile (get-environment-variable "USERPROFILE"))
      (args (parse-pi-args '()))
      (home (path-join (path-temp-dir) "pi-config-dir-resolve-home-fallback"))
     ) ;
  (unsetenv "PI_CODING_AGENT_DIR")
  (path-mkdir (path-join home ".pi" "agent") :parents #t :exist-ok #t)
  (%write-json! (path-join home ".pi" "agent" "settings.json")
    (list (cons "defaultProvider" "google") (cons "defaultModel" "gemini-3.8-flash")) ;list
  ) ;%write-json!
  (putenv "HOME" (path->string home))
  (when (os-windows?) (putenv "USERPROFILE" (path->string home)))
  (let* ((cfg (resolve-pi-config args '()))
         (config-dir (cdr (assoc 'config-dir cfg)))
        ) ;
    (check-true (string-ends? (%normalize config-dir) "pi-config-dir-resolve-home-fallback/.gf/agent"))
  ) ;let*
  (if saved-agent
    (putenv "PI_CODING_AGENT_DIR" saved-agent)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (if saved-home
    (putenv "HOME" saved-home)
    (unsetenv "HOME")
  ) ;if
  (when (os-windows?)
    (if saved-profile
      (putenv "USERPROFILE" saved-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
  (rmtree (path->string home) #t)
) ;let

(rmtree (path->string HOME_DIR) #t)

(check-report)

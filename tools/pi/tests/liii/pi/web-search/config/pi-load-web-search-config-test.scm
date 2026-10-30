(import (scheme base)
        (scheme process-context)
        (liii check)
        (liii string)
        (liii os)
        (liii path)
        (liii shutil)
        (liii json)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-load-web-search-config
;; 从显式路径加载
(define (find-web-search-fixture)
  (let loop ((candidates '("tools/pi/tests/fixtures/web-search.json"
                           "tests/fixtures/web-search.json"
                           "fixtures/web-search.json"
                           "../fixtures/web-search.json"
                           "../../fixtures/web-search.json"
                           "../../../fixtures/web-search.json"
                           "../../../../tools/pi/tests/fixtures/web-search.json")))
    (cond ((null? candidates) (error 'file-not-found "web-search.json fixture not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates))))))

(let ((cfg (pi-load-web-search-config
             (list (cons "PI_WEB_SEARCH_CONFIG" (find-web-search-fixture))
               (cons "HOME" "/nonexistent-gf-test")
               (cons "PI_CODING_AGENT_DIR" "/nonexistent-gf-test")))))
  (check-true (pair? cfg))
  (check (cdr (assoc "provider" cfg)) => "deepseek"))

;; 路径不存在时返回 #f
(check-false (pi-load-web-search-config
               '(("PI_WEB_SEARCH_CONFIG" . "/nonexistent-gf-test/ws.json")
                 ("HOME" . "/nonexistent-gf-test")
                 ("PI_CODING_AGENT_DIR" . "/nonexistent-gf-test"))))

;; 默认目录：~/.gf/agent 存在时优先于 ~/.pi/agent
(let* ((home (path-join (path-temp-dir) "pi-web-search-config-home"))
       (gf-agent (path-join home ".gf" "agent"))
       (pi-agent (path-join home ".pi" "agent"))
       (saved-agent (get-environment-variable "PI_CODING_AGENT_DIR"))
       (saved-home (get-environment-variable "HOME"))
       (saved-profile (get-environment-variable "USERPROFILE"))
      ) ;
  (path-mkdir gf-agent :parents #t :exist-ok #t)
  (path-mkdir pi-agent :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join gf-agent "web-search.json"))
    (string-append (json->string (list (cons "provider" "google") (cons "model" "gemini-2.5-flash"))) "\n")
  ) ;path-write-text
  (path-write-text (path->string (path-join pi-agent "web-search.json"))
    (string-append (json->string (list (cons "provider" "deepseek") (cons "model" "deepseek-chat"))) "\n")
  ) ;path-write-text
  (unsetenv "PI_CODING_AGENT_DIR")
  (putenv "HOME" (path->string home))
  (when (os-windows?) (putenv "USERPROFILE" (path->string home)))
  (let ((cfg (pi-load-web-search-config '())))
    (check-true (pair? cfg))
    (check (cdr (assoc "provider" cfg)) => "google")
    (check (cdr (assoc "model" cfg)) => "gemini-2.5-flash")
  ) ;let
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

(check-report)

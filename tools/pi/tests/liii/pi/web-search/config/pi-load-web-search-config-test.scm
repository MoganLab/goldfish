;; pi-load-web-search-config
;; 从配置目录读取 web-search.json 配置文件。
;;
;; 语法
;; ----
;; (pi-load-web-search-config . opts)
;;
;; 参数
;; ----
;; opts : list?
;; 可选关键字参数，支持 :config-dir、:env 等。
;;
;; 返回值
;; ----
;; list?
;; 解析后的配置 alist，未配置时返回空列表。

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

;; 读取 web-search.json 配置文件。
;; 从显式路径加载

(define (find-web-search-fixture)
  (let loop
    ((candidates '("tools/pi/tests/fixtures/web-search.json"
                   "tests/fixtures/web-search.json" "fixtures/web-search.json"
                   "../fixtures/web-search.json"
                   "../../fixtures/web-search.json"
                   "../../../fixtures/web-search.json"
                   "../../../../tools/pi/tests/fixtures/web-search.json")
     ) ;candidates
    ) ;
    (cond ((null? candidates) (error 'file-not-found "web-search.json fixture not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(let ((cfg
        (pi-load-web-search-config (list (cons "PI_WEB_SEARCH_CONFIG" (find-web-search-fixture))
                                     (cons "HOME" "/nonexistent-gf-test")
                                     (cons "PI_CODING_AGENT_DIR" "/nonexistent-gf-test")
                                   ) ;list
        ) ;pi-load-web-search-config
      ) ;cfg
     ) ;
  (check-true (pair? cfg))
  (check (cdr (assoc "provider" cfg)) => "deepseek")
) ;let

;; 路径不存在时返回 #f
(check-false (pi-load-web-search-config '(("PI_WEB_SEARCH_CONFIG"
                                           . "/nonexistent-gf-test/ws.json")
                                          ("HOME" . "/nonexistent-gf-test")
                                          ("PI_CODING_AGENT_DIR"
                                           . "/nonexistent-gf-test"))
             ) ;pi-load-web-search-config
) ;check-false

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
    (string-append (json->string (list (cons "provider" "google") (cons "model" "gemini-2.5-flash"))
                   ) ;json->string
      "\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text (path->string (path-join pi-agent "web-search.json"))
    (string-append (json->string (list (cons "provider" "deepseek") (cons "model" "deepseek-chat"))
                   ) ;json->string
      "\n"
    ) ;string-append
  ) ;path-write-text
  (unsetenv "PI_CODING_AGENT_DIR")
  (putenv "HOME" (path->string home))
  (when (os-windows?)
    (putenv "USERPROFILE" (path->string home))
  ) ;when
  (let ((cfg (pi-load-web-search-config '())))
    (check-true (pair? cfg))
    (check (cdr (assoc "provider" cfg)) => "google")
    (check (cdr (assoc "model" cfg)) => "gemini-2.5-flash")
  ) ;let
  (if saved-agent
    (putenv "PI_CODING_AGENT_DIR" saved-agent)
    (unsetenv "PI_CODING_AGENT_DIR")
  ) ;if
  (if saved-home (putenv "HOME" saved-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if saved-profile (putenv "USERPROFILE" saved-profile) (unsetenv "USERPROFILE"))
  ) ;when
  (rmtree (path->string home) #t)
) ;let*

(check-report)

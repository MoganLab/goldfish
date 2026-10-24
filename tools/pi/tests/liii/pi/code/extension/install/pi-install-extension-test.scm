(import (scheme base)
  (liii check)
  (liii path)
  (liii os)
  (liii subprocess)
  (liii either)
  (liii pi code extension install)
  (liii pi code extension discovery)
) ;import

(define (rmdir-rec d)
  (when (path-dir? d)
    (for-each (lambda (e)
                (let ((full (path-join d e)))
                  (if (path-dir? full) (rmdir-rec full) (remove (path->string full)))
                ) ;let
              ) ;lambda
      (vector->list (path-list d))
    ) ;for-each
    (rmdir (path->string d))
  ) ;when
) ;define

(let* ((temp-dir (path-join (path-temp-dir) (string-append "gf-install-flow-" (number->string (getpid))))))
  (when (path-exists? temp-dir) (rmdir-rec temp-dir))
  (path-mkdir temp-dir :parents #t :exist-ok #t)

  (let* ((source-repo (path-join temp-dir "sample-plugin"))
         (source-repo-str (path->string source-repo))
         (agent-dir (path-join temp-dir "mock-agent"))
         (agent-dir-str (path->string agent-dir))
         (project-dir (path-join temp-dir "mock-project"))
         (project-dir-str (path->string project-dir))
        ) ;
    (path-mkdir source-repo :parents #t :exist-ok #t)
    (run-either (list "git" "init") :cwd source-repo-str :timeout 30)
    (path-write-text (path-join source-repo "index.scm") ";; sample\n")
    (run-either (list "git" "config" "user.name" "Test") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "config" "user.email" "test@example.com") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "add" ".") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "commit" "-m" "initial") :cwd source-repo-str :timeout 30)

    ;; 1. 执行全局安装
    (let ((installed (pi-install-extension source-repo-str :local? #f :agent-dir agent-dir-str :cwd project-dir-str :timeout 30)))
      (check-true (string? installed))
      (check-true (path-exists? (path installed)))
      ;; 验证能被 pi-discover-extensions 发现
      (let ((discovered (pi-discover-extensions :agent-dir agent-dir-str :cwd project-dir-str)))
        (check-true (pair? (member (path->string (path-resolve (path-join (path installed) "index.scm"))) discovered)))
      ) ;let
    ) ;let

    ;; 2. 执行项目局部安装 (--local)
    (let ((local-installed (pi-install-extension source-repo-str :local? #t :agent-dir agent-dir-str :cwd project-dir-str :timeout 30)))
      (check-true (string? local-installed))
      (check-true (path-exists? (path local-installed)))
      (let ((discovered (pi-discover-extensions :agent-dir agent-dir-str :cwd project-dir-str)))
        (check-true (pair? (member (path->string (path-resolve (path-join (path local-installed) "index.scm"))) discovered)))
      ) ;let
    ) ;let
  ) ;let*

  (rmdir-rec temp-dir)
) ;let*

(check-report)

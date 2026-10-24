(import (scheme base)
  (liii check)
  (liii path)
  (liii os)
  (liii subprocess)
  (liii either)
  (liii pi code extension install)
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

(let* ((temp-dir (path-join (path-temp-dir) (string-append "gf-install-test-" (number->string (getpid))))))
  (when (path-exists? temp-dir) (rmdir-rec temp-dir))
  (path-mkdir temp-dir :parents #t :exist-ok #t)

  ;; 1. 创建一个本地 mock git 仓库作为源
  (let* ((source-repo (path-join temp-dir "mock-remote"))
         (source-repo-str (path->string source-repo))
         (dest-dir (path-join temp-dir "installed-ext"))
         (dest-dir-str (path->string dest-dir))
        ) ;
    (path-mkdir source-repo :parents #t :exist-ok #t)
    (let ((init-res (run-either (list "git" "init") :cwd source-repo-str :timeout 30)))
      (check-true (either-right? init-res))
    ) ;let
    (path-write-text (path-join source-repo "index.scm") "(define (init) 'ok)\n")
    (run-either (list "git" "config" "user.name" "Test") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "config" "user.email" "test@example.com") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "add" ".") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "commit" "-m" "initial") :cwd source-repo-str :timeout 30)

    ;; 2. 测试克隆安装成功
    (let ((res (pi-git-clone-extension source-repo-str dest-dir-str :timeout 30)))
      (check res => #t)
      (check-true (path-exists? (path-join dest-dir "index.scm")))
    ) ;let

    ;; 3. 目标目录已存在时抛出 conflict-error 或 value-error
    (check-catch 'value-error (pi-git-clone-extension source-repo-str dest-dir-str :timeout 30))

    ;; 4. 克隆失败时自动清理半成品目标目录
    (let* ((bad-dest (path-join temp-dir "failed-dest"))
           (bad-dest-str (path->string bad-dest))
          ) ;
      ;; 传入不存在的仓库
      (check-catch 'install-error
        (pi-git-clone-extension "file:///non/existent/repo.git" bad-dest-str :timeout 10)
      ) ;check-catch
      ;; 验证半成品已被清理
      (check-false (path-exists? bad-dest))
    ) ;let
  ) ;let*

  ;; 清理测试环境
  (rmdir-rec temp-dir)
) ;let*

(check-report)

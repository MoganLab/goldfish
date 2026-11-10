;; pi-git-clone-extension
;; 将 Git 仓库克隆到扩展目录。
;;
;; 语法
;; ----
;; (pi-git-clone-extension url dest-dir-str . opts)
;;
;; 参数
;; ----
;; url : string
;; Git 仓库地址。
;;
;; dest-dir-str : string
;; 克隆目标目录。
;;
;; opts : list?
;; 可选关键字参数，支持 :timeout 等。
;;
;; 返回值
;; ----
;; boolean
;; 克隆成功返回 #t。

(import (scheme base)
  (liii check)
  (liii check)
  (liii path)
  (liii os)
  (liii subprocess)
  (liii either)
  (liii pi code core extensions install)
) ;import

(define (rmdir-rec d)
  (when (path-dir? d)
    (for-each
      (lambda (e)
        (let ((full (path-join d e)))
          (if (path-dir? full) (rmdir-rec full) (remove (path->string full)))
        ) ;let
      ) ;lambda
      (vector->list (path-list d))
    ) ;for-each
    (rmdir (path->string d))
  ) ;when
) ;define

(let* ((temp-dir
         (path-join (path-temp-dir)
           (string-append "gf-install-test-" (number->string (getpid)))
         ) ;path-join
       ) ;temp-dir
      ) ;
  (when (path-exists? temp-dir)
    (rmdir-rec temp-dir)
  ) ;when
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
    (run-either (list "git" "config" "user.name" "Test")
      :cwd     source-repo-str
      :timeout 30
    ) ;run-either
    (run-either (list "git" "config" "user.email" "test@example.com")
      :cwd     source-repo-str
      :timeout 30
    ) ;run-either
    (run-either (list "git" "add" ".") :cwd source-repo-str :timeout 30)
    (run-either (list "git" "commit" "-m" "initial")
      :cwd     source-repo-str
      :timeout 30
    ) ;run-either

    ;; 2. 测试克隆安装成功
    (let ((res (pi-git-clone-extension source-repo-str dest-dir-str :timeout 30)))
      (check res => #t)
      (check-true (path-exists? (path-join dest-dir "index.scm")))
    ) ;let

    ;; 3. 目标目录已存在时抛出 conflict-error 或 value-error
    (check-catch 'value-error
      (pi-git-clone-extension source-repo-str dest-dir-str :timeout 30)
    ) ;check-catch

    ;; 4. 克隆失败时自动清理半成品目标目录
    (let* ((bad-dest (path-join temp-dir "failed-dest"))
           (bad-dest-str (path->string bad-dest))
          ) ;
      ;; 传入不存在的仓库
      (check-catch 'install-error
        (pi-git-clone-extension "file:///non/existent/repo.git"
          bad-dest-str
          :timeout 10
        ) ;pi-git-clone-extension
      ) ;check-catch
      ;; 验证半成品已被清理
      (check-false (path-exists? bad-dest))
    ) ;let*
  ) ;let*

  ;; 清理测试环境
  (rmdir-rec temp-dir)
) ;let*

(check-report)

;; get-git-commit
;; 解析目录中 git 仓库的 HEAD commit。
;;
;; 语法
;; ----
;; (get-git-commit dir)
;;
;; 参数
;; ----
;; dir : path?
;; git 工作区目录。
;;
;; 返回值
;; ----
;; string
;; 40 位十六进制 commit sha；非 git 目录抛 os-error。

(import (liii check) (liii os) (liii path) (liii shutil) (liii string)
  (liii subprocess) (liii gfproject get fetch)
) ;import

(check-set-mode! 'report-failed)

(define base (path-join (path-temp-dir)
               (string-append "gf-get-commit-" (number->string (getpid)))
             ) ;path-join
) ;define

(define (git! args cwd)
  (let ((r (run-either (cons "git" args) :cwd cwd)))
    (check-true (and (pair? r) (eq? (cdr r) 'right)))
  ) ;let
) ;define

(path-mkdir base #t #t)
(path-write-text (path-join base "a.txt") "x\n")
(git! (list "init" "-q" "-b" "main") (path->string base))
(git! (list "add" ".") (path->string base))
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
  (path->string base)
) ;git!

(let ((sha (get-git-commit base)))
  (check (string-length sha) => 40)
  (check-true (string-every
                (lambda (c)
                  (or (char<=? #\0 c #\9) (char<=? #\a c #\f))
                ) ;lambda
                sha
              ) ;string-every
  ) ;check-true
) ;let

(rmtree (path->string base) #t)

;; 非 git 目录
(check-catch 'os-error (get-git-commit (path-temp-dir)))

(check-report)

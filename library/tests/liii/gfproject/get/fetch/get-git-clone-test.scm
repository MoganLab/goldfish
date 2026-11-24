;; get-git-clone
;; 浅克隆仓库到临时目录；失败时清理临时目录并抛 os-error。
;;
;; 语法
;; ----
;; (get-git-clone url)
;;
;; 参数
;; ----
;; url : string?
;; 仓库 URL（测试使用本地 file:// 裸仓库）。
;;
;; 返回值
;; ----
;; path?
;; 克隆出的临时目录，调用方负责清理。

(import (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii subprocess)
  (liii gfproject get fetch)
) ;import

(check-set-mode! 'report-failed)

(define base
  (path-join (path-temp-dir)
    (string-append "gf-get-clone-" (number->string (getpid)))
  ) ;path-join
) ;define

(define work (path-join base "work"))

(define bare (path-join base "remote.git"))

(define (git! args cwd)
  (let ((r (run-either (cons "git" args) :cwd cwd)))
    (check-true (and (pair? r) (eq? (cdr r) 'right)))
  ) ;let
) ;define

(path-mkdir work #t #t)
(path-write-text (path-join work "a.txt") "hello\n")
(git! (list "init" "-q" "-b" "main") (path->string work))
(git! (list "add" ".") (path->string work))
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
  (path->string work)
) ;git!
(git! (list "clone" "-q" "--bare" (path->string work) (path->string bare)) #f)
(git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare))

;; 克隆成功：临时目录含工作区文件
;; HEAD 安装：默认浅克隆
(let ((tmp (get-git-clone (string-append "file:///" (path->string bare)) #f)))
  (check-true (path-dir? tmp))
  (check-true (path-file? (path-join tmp "a.txt")))
  (rmtree (path->string tmp) #t)
) ;let

;; 分支 ref：clone --branch <branch> 检出指定分支
(let ((tmp (get-git-clone (string-append "file:///" (path->string bare)) "main")))
  (check-true (path-dir? tmp))
  (check-true (path-file? (path-join tmp "a.txt")))
  (rmtree (path->string tmp) #t)
) ;let

;; commit ref：回退全量 clone + checkout 到指定 commit
(let* ((work2 (path-join base "work2"))
       (bare2 (path-join base "remote2.git"))
       (c1 #f)
       (c2 #f)
      ) ;
  (path-mkdir work2 #t #t)
  (path-write-text (path-join work2 "a.txt") "1\n")
  (git! (list "init" "-q" "-b" "main") (path->string work2))
  (git! (list "add" ".") (path->string work2))
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "c1")
    (path->string work2)
  ) ;git!
  (set! c1 (get-git-commit work2))
  (path-write-text (path-join work2 "a.txt") "2\n")
  (git! (list "add" ".") (path->string work2))
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "c2")
    (path->string work2)
  ) ;git!
  (set! c2 (get-git-commit work2))
  (git! (list "clone" "-q" "--bare" (path->string work2) (path->string bare2)) #f)
  (git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare2))
  ;; 检出第一个 commit（与 HEAD 不同）
  (let ((tmp (get-git-clone (string-append "file:///" (path->string bare2)) c1)))
    (check-true (path-dir? tmp))
    (check (path-read-text (path-join tmp "a.txt")) => "1\n")
    (rmtree (path->string tmp) #t)
  ) ;let
) ;let*

;; 不存在的 ref：抛 os-error
(check-catch 'os-error
  (get-git-clone (string-append "file:///" (path->string bare)) "no-such-ref")
) ;check-catch

;; 非法 URL：抛 os-error 且不残留临时目录
(check-catch 'os-error (get-git-clone "file:///nonexistent/repo.git" #f))
(check-catch 'type-error (get-git-clone "" #f))

(rmtree (path->string base) #t)

(check-report)

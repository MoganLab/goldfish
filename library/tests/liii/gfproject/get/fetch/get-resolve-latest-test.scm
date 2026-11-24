;; get-resolve-latest
;; 解析远端仓库的最新稳定 release tag（@latest 伪 ref 的实现）。
;;
;; 语法
;; ----
;; (get-resolve-latest url)
;;
;; 参数
;; ----
;; url : string?
;; 仓库 URL（测试使用本地 file:// 裸仓库）。
;;
;; 返回值
;; ----
;; string
;; 最新稳定（非 pre-release）semver tag 的原始名（保留 v 前缀）。
;; 无任何稳定 tag 时抛 value-error。

(import (liii check) (liii os) (liii path) (liii shutil) (liii string)
  (liii subprocess) (liii gfproject get fetch)
) ;import

(check-set-mode! 'report-failed)

(define base (path-join (path-temp-dir)
               (string-append "gf-get-latest-" (number->string (getpid)))
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
(path-write-text (path-join work "a.txt") "x\n")
(git! (list "init" "-q" "-b" "main") (path->string work))
(git! (list "add" ".") (path->string work))
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
  (path->string work)
) ;git!

;; 打一组 tag：稳定、pre-release、非 semver 混杂
(git! (list "tag" "v1.0.0") (path->string work))
(git! (list "tag" "v1.2.0") (path->string work))
(git! (list "tag" "v1.10.0") (path->string work))
(git! (list "tag" "v2.0.0-rc.1") (path->string work))
(git! (list "tag" "v2.0.0-beta") (path->string work))
(git! (list "tag" "nightly") (path->string work))
(git! (list "tag" "not-a-version") (path->string work))

(git! (list "clone" "-q" "--bare" (path->string work) (path->string bare)) #f)
(git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare))

(define url (string-append "file:///" (path->string bare)))

;; 取最高稳定 tag：v1.10.0 > v1.2.0 > v1.0.0，排除 v2.0.0-rc.1 / -beta / nightly
(check (get-resolve-latest url) => "v1.10.0")

;; prerelease 不得被选中（最高稳定版，而非含 pre-release 的最高版）
(check-false (string-contains? (get-resolve-latest url) "-"))

;; 空 url：type-error
(check-catch 'type-error (get-resolve-latest ""))

;; 仅有 pre-release / 非 semver tag 的仓库：无稳定 tag，抛 value-error
(let* ((work2 (path-join base "work2"))
       (bare2 (path-join base "remote2.git"))
      ) ;
  (path-mkdir work2 #t #t)
  (path-write-text (path-join work2 "a.txt") "y\n")
  (git! (list "init" "-q" "-b" "main") (path->string work2))
  (git! (list "add" ".") (path->string work2))
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
    (path->string work2)
  ) ;git!
  (git! (list "tag" "v0.1.0-rc.1") (path->string work2))
  (git! (list "tag" "dev") (path->string work2))
  (git! (list "clone" "-q" "--bare" (path->string work2) (path->string bare2)) #f)
  (git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare2))
  (check-catch 'value-error
    (get-resolve-latest (string-append "file:///" (path->string bare2)))
  ) ;check-catch
) ;let*

;; 非法 URL：git ls-remote 失败抛 os-error
(check-catch 'os-error (get-resolve-latest "file:///nonexistent/repo.git"))

(rmtree (path->string base) #t)

(check-report)

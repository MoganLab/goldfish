;; gfproject-remove-package
;; 卸载用户级包（gf get -g 的逆操作）：删远程安装目录（本地开发包
;; 只清注册，不动工作副本）、清 depends/lock 条目、注销自有工具绑定。
;;
;; 语法
;; ----
;; (gfproject-remove-package id)
;;
;; 参数
;; ----
;; id : string?
;; 已注册短包名 name@organization。
;;
;; 返回值
;; ----
;; integer
;; 成功返回 0；未注册/非法 id 抛异常。
;;
;; 说明
;; ----
;; 测试在隔离 HOME 下运行：先用 get-install 安装本地开发包与远程包
;; （file:// 本地裸仓库 + 稳定 tag，缺省 @latest），再验证卸载行为。

(import (scheme base)
  (liii check)
  (liii json)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii subprocess)
  (liii gfproject get install)
  (liii gfproject get store)
  (liii gfproject remove)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))

(define orig-profile (getenv "USERPROFILE"))

(define orig-cwd (getcwd))

(define (set-test-home! dir)
  (putenv "HOME" dir)
  (when (os-windows?)
    (putenv "USERPROFILE" dir)
  ) ;when
) ;define

(define (restore-test-home!)
  (if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if orig-profile (putenv "USERPROFILE" orig-profile) (unsetenv "USERPROFILE"))
  ) ;when
) ;define

(define base
  ;; 预先 realpath，与 get-source-local-path 经 path-resolve 的记录一致（macOS /var 符号链接）。
  (path-realpath
    (path-join (path-temp-dir)
      (string-append "gf-remove-" (number->string (getpid)))
    ) ;path-join
  ) ;path-realpath
) ;define

(define home-dir (path-join base "home"))

(define proj (path-join base "proj"))

(define (write-text p text)
  (path-mkdir (path-parent p) #t #t)
  (path-write-text p text)
) ;define

(define (make-project! root)
  (write-text (path-join root "gfproject.json")
    (string-append "{\"package\":{\"name\":\"demo\",\"organization\":\"liii\",\"description\":\"Demo\"},"
      "\"domains\":{\"cli\":{\"root\":\"cli/\"}},"
      "\"tools\":{\"demo\":{\"organization\":\"liii\",\"module\":\"demo/cli\"}}}"
    ) ;string-append
  ) ;write-text
  (write-text (path-join root "cli" "liii" "demo" "cli.scm")
    "(define-library (liii demo cli) (import (scheme base)) (export main) (begin (define (main) 0)))\n"
  ) ;write-text
) ;define

(path-mkdir home-dir #t #t)
(make-project! proj)
(set-test-home! (path->string home-dir))

;; 1. 本地开发包卸载：清注册与工具绑定，工作副本保留
(check (get-install (path->string proj) #f #f #f) => 0)
(check (gfproject-remove-package "demo@liii") => 0)
(let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
       (lock (get-read-json (get-lock-path)))
      ) ;
  (check-false (get-depends-entry config "demo@liii"))
  (check-false (get-lock-entry lock "demo@liii"))
  (check-false (json-contains-key? (get-json-sub config "tools") "demo"))
  ;; 工作副本不动
  (check-true (path-file? (path-join proj "gfproject.json")))
  (check-true (path-file? (path-join proj "cli" "liii" "demo" "cli.scm")))
) ;let*

;; 2. 未注册包报错；非法 id（URL/路径/无 @）报错
(check-catch 'value-error (gfproject-remove-package "nosuch@liii"))
(check-catch 'value-error (gfproject-remove-package "https://example.com/r.git"))
(check-catch 'value-error (gfproject-remove-package (path->string proj)))
(check-catch 'value-error (gfproject-remove-package "demo"))

;; 3. 远程包卸载：删安装目录及已空的 <name>/<org> 目录，清 depends/lock 与工具
(define (git! args cwd)
  (let ((r (run-either (cons "git" args) :cwd cwd)))
    (check-true (and (pair? r) (eq? (cdr r) 'right)))
  ) ;let
) ;define

(define bare (path-join base "remote.git"))

(git! (list "init" "-q" "-b" "main") (path->string proj))
(git! (list "add" ".") (path->string proj))
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
  (path->string proj)
) ;git!
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "tag" "v1.0.0")
  (path->string proj)
) ;git!
(git! (list "clone" "-q" "--bare" (path->string proj) (path->string bare)) #f)
(git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare))
(git! (list "push" "-q" (path->string bare) "refs/tags/v1.0.0")
  (path->string proj)
) ;git!

(let ((url (string-append "file:///" (path->string bare))))
  ;; 缺省 --ref @latest 安装
  (check (get-install url #f #f #t) => 0)
  (let ((root (get-package-root "demo@liii")))
    (check-true (path-file? (path-join root "gfproject.json")))
    (check (gfproject-remove-package "demo@liii") => 0)
    (check-false (path-dir? root))
    ;; 已空的 <name> 与 <org> 目录一并清理，site-lisp 根保留
    (check-false (path-dir? (path-parent root)))
    (check-false (path-dir? (path-parent (path-parent root))))
    (check-true (path-dir? (get-site-lisp-dir)))
  ) ;let
  (let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
         (lock (get-read-json (get-lock-path)))
        ) ;
    (check-false (get-depends-entry config "demo@liii"))
    (check-false (get-lock-entry lock "demo@liii"))
    (check-false (json-contains-key? (get-json-sub config "tools") "demo"))
  ) ;let*
  ;; 卸载后可重新安装（注册表干净）
  (check (get-install url #f #f #f) => 0)
  (check (gfproject-remove-package "demo@liii") => 0)
) ;let

;; 4. 只注销自有工具：他人归属与无 package 绑定的工具保留
(check (get-install (path->string proj) #f #f #f) => 0)
(let* ((config-path (path-join (get-user-dir) "gfproject.json"))
       (config (get-read-json config-path))
       (tools (get-json-sub config "tools"))
       (tools (get-json-set tools
                "foreign"
                (get-json-set (get-json-set '(()) "package" "other@liii") "module" "other/cli")
              ) ;get-json-set
       ) ;tools
       (tools (get-json-set tools "manual" (get-json-set '(()) "module" "manual/cli")))
      ) ;
  (get-write-json config-path (get-json-set config "tools" tools))
  (check (gfproject-remove-package "demo@liii") => 0)
  (let ((tools (get-json-sub (get-read-json config-path) "tools")))
    (check-false (json-contains-key? tools "demo"))
    (check-true (json-contains-key? tools "foreign"))
    (check-true (json-contains-key? tools "manual"))
  ) ;let
) ;let*

(restore-test-home!)
(chdir orig-cwd)
(rmtree (path->string base) #t)

(check-report)

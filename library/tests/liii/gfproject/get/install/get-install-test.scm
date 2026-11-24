;; get-install
;; 用户级安装编排：本地开发模式与远程 HEAD 安装（file:// 本地裸仓库）。
;;
;; 语法
;; ----
;; (get-install arg from force?)
;;
;; 参数
;; ----
;; arg : string?
;; 来源（URL/路径）或已注册短包名。
;;
;; from : string? or #f
;; --from 显式来源。
;;
;; force? : boolean?
;; 冲突时是否强制替换。
;;
;; 返回值
;; ----
;; integer
;; 成功返回 0；失败抛异常。
;;
;; 说明
;; ----
;; 测试在隔离 HOME 下运行：临时项目含 package 元数据、两个 domain
;; 与一个工具；远程用例先 git init/commit 到工作仓库再 clone --bare。

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
  (liii gfproject package)
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
  ;; macOS 上系统临时目录 /var 是 /private/var 的符号链接，而
  ;; get-source-local-path 经 path-resolve 解析后存入 depends 的是物理路径。
  ;; 此处预先 realpath，使后续路径比较与安装记录一致。
  (path-realpath
    (path-join (path-temp-dir)
      (string-append "gf-get-install-" (number->string (getpid)))
    ) ;path-join
  ) ;path-realpath
) ;define

(define home-dir (path-join base "home"))

(define proj (path-join base "proj"))

(define (write-text p text)
  (path-mkdir (path-parent p) #t #t)
  (path-write-text p text)
) ;define

;; 构造带 package/domains/tools 的最小项目

(define (make-project! root)
  (write-text (path-join root "gfproject.json")
    (string-append "{\"package\":{\"name\":\"demo\",\"organization\":\"liii\",\"description\":\"Demo\"},"
      "\"domains\":{\"base\":{\"root\":\"base/\"},\"cli\":{\"root\":\"cli/\",\"depends-on\":[\"base\"]}},"
      "\"tools\":{\"demo\":{\"organization\":\"liii\",\"module\":\"demo/cli\"}}}"
    ) ;string-append
  ) ;write-text
  (write-text (path-join root "base" "liii" "demo" "base.scm")
    "(define-library (liii demo base) (import (scheme base)) (export demo-value) (begin (define demo-value 7)))\n"
  ) ;write-text
  (write-text (path-join root "cli" "liii" "demo" "cli.scm")
    "(define-library (liii demo cli) (import (scheme base) (liii demo base)) (export main) (begin (define (main) (if (= demo-value 7) 0 1))))\n"
  ) ;write-text
) ;define

(path-mkdir home-dir #t #t)
(make-project! proj)
(set-test-home! (path->string home-dir))

;; 1. 本地开发安装：depends 记 path/local，lock 记 version=dev，tools 带 package 绑定
(check (get-install (path->string proj) #f #f) => 0)
(let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
       (entry (get-depends-entry config "demo@liii"))
       (lock (get-read-json (get-lock-path)))
       (fact (get-lock-entry lock "demo@liii"))
       (tool (json-ref (json-ref config "tools") "demo"))
      ) ;
  (check (json-ref-string entry "path" "") => (path->string proj))
  (check (json-ref entry "local") => #t)
  (check (json-ref-string fact "version" "") => "dev")
  (check (json-ref-string tool "package" "") => "demo@liii")
  (check (json-ref-string tool "module" "") => "demo/cli")
  ;; 本地包不复制进 site-lisp
  (check-false
    (path-dir?
      (get-package-dir (json->gfproject-package (get-read-json (path-join proj "gfproject.json")))
        "HEAD"
      ) ;get-package-dir
    ) ;path-dir?
  ) ;check-false
) ;let*

;; 2. 幂等重装：depends/tools 不产生重复键
(check (get-install "demo@liii" #f #f) => 0)
(let ((config (get-read-json (path-join (get-user-dir) "gfproject.json"))))
  (check (length (json-keys (json-ref config "depends"))) => 1)
  (check (length (json-keys (json-ref config "tools"))) => 1)
) ;let

;; 3. --from 身份校验：请求身份与仓库声明不符时拒绝
(check-catch 'value-error (get-install "other@liii" (path->string proj) #f))

;; 4. 来源冲突：同 id 不同来源拒绝，--force 允许
(let ((proj2 (path-join base "proj2")))
  (make-project! proj2)
  (check-catch 'value-error (get-install (path->string proj2) #f #f))
  (check (get-install (path->string proj2) #f #t) => 0)
) ;let

;; 5. 远程 HEAD 安装：init/commit → bare → file:// clone

(define bare (path-join base "remote.git"))

(define (git! args cwd)
  (let ((r (run-either (cons "git" args) :cwd cwd)))
    (check-true (and (pair? r) (eq? (cdr r) 'right)))
  ) ;let
) ;define
(git! (list "init" "-q" "-b" "main") (path->string proj))
(git! (list "add" ".") (path->string proj))
(git! (list "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "-m" "init")
  (path->string proj)
) ;git!
(git! (list "clone" "-q" "--bare" (path->string proj) (path->string bare)) #f)
;; bare 默认 HEAD 可能指向 master，对齐到 main
(git! (list "symbolic-ref" "HEAD" "refs/heads/main") (path->string bare))

(let ((url (string-append "file:///" (path->string bare))))
  (check (get-install url #f #t) => 0)
  (let* ((lock (get-read-json (get-lock-path)))
         (fact (get-lock-entry lock "demo@liii"))
         (root (get-package-root "demo@liii"))
        ) ;
    (check (json-ref-string fact "version" "") => "HEAD")
    (check-true (> (string-length (json-ref-string fact "commit" "")) 0))
    (check-true (path-file? (path-join root "gfproject.json")))
    (check-true (path-file? (path-join root "cli" "liii" "demo" "cli.scm")))
    ;; 短名重装（更新路径）成功且 commit 稳定
    (check (get-install "demo@liii" #f #f) => 0)
    (check
      (json-ref-string (get-lock-entry (get-read-json (get-lock-path)) "demo@liii")
        "commit"
        ""
      ) ;json-ref-string
      =>
      (json-ref-string fact "commit" "")
    ) ;check
  ) ;let*
) ;let

;; 6. 未注册短名报错并提示首次安装方式
(check-catch 'value-error (get-install "nosuch@liii" #f #f))

(restore-test-home!)
(chdir orig-cwd)
(rmtree (path->string base) #t)

(check-report)

;; get-install
;; 用户级安装编排：本地开发模式与远程 HEAD 安装（file:// 本地裸仓库）。
;;
;; 语法
;; ----
;; (get-install arg from ref force?)
;;
;; 参数
;; ----
;; arg : string?
;; 来源（URL/路径）或已注册短包名。
;;
;; from : string? or #f
;; --from 显式来源。
;;
;; ref : string? or #f
;; git 引用（分支/标签/commit）或 @latest（最新稳定 release tag）；
;; 仅对远程 url 生效，本地路径忽略。
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
;; ref 用例覆盖分支、commit 检出与冲突校验。

(import (scheme base)
  (liii check)
  (liii json)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii subprocess)
  (liii gfproject get install)
  (liii gfproject get fetch)
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
(check (get-install (path->string proj) #f #f #f) => 0)
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
(check (get-install "demo@liii" #f #f #f) => 0)
(let ((config (get-read-json (path-join (get-user-dir) "gfproject.json"))))
  (check (length (json-keys (json-ref config "depends"))) => 1)
  (check (length (json-keys (json-ref config "tools"))) => 1)
) ;let

;; 3. --from 身份校验：请求身份与仓库声明不符时拒绝
(check-catch 'value-error (get-install "other@liii" (path->string proj) #f #f))

;; 4. 来源冲突：同 id 不同来源拒绝，--force 允许
(let ((proj2 (path-join base "proj2")))
  (make-project! proj2)
  (check-catch 'value-error (get-install (path->string proj2) #f #f #f))
  (check (get-install (path->string proj2) #f #f #t) => 0)
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
  (check (get-install url #f #f #t) => 0)
  (let* ((lock (get-read-json (get-lock-path)))
         (fact (get-lock-entry lock "demo@liii"))
         (root (get-package-root "demo@liii"))
        ) ;
    (check (json-ref-string fact "version" "") => "HEAD")
    (check-true (> (string-length (json-ref-string fact "commit" "")) 0))
    (check-true (path-file? (path-join root "gfproject.json")))
    (check-true (path-file? (path-join root "cli" "liii" "demo" "cli.scm")))
    ;; 短名重装（更新路径）成功且 commit 稳定
    (check (get-install "demo@liii" #f #f #f) => 0)
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
(check-catch 'value-error (get-install "nosuch@liii" #f #f #f))

;; 7. --ref 分支安装：version/ref 入 depends 与 lock，安装目录为 <ref>
(let ((url (string-append "file:///" (path->string bare))))
  (check (get-install url #f "main" #t) => 0)
  (let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
         (entry (get-depends-entry config "demo@liii"))
         (lock (get-read-json (get-lock-path)))
         (fact (get-lock-entry lock "demo@liii"))
         (root (get-package-root "demo@liii"))
        ) ;
    (check (json-ref-string entry "ref" "") => "main")
    (check (json-ref-string fact "version" "") => "main")
    (check (json-ref-string fact "ref" "") => "main")
    (check-true (> (string-length (json-ref-string fact "commit" "")) 0))
    (check-true (path-file? (path-join root "gfproject.json")))
  ) ;let*
  ;; 短名重装复用登记的 ref
  (check (get-install "demo@liii" #f #f #f) => 0)
  (let ((fact (get-lock-entry (get-read-json (get-lock-path)) "demo@liii")))
    (check (json-ref-string fact "ref" "") => "main")
    (check (json-ref-string fact "version" "") => "main")
  ) ;let
) ;let

;; 8. --ref tag 安装
(let ((url (string-append "file:///" (path->string bare))))
  ;; 为工作仓库打 tag 并推送到 bare
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "tag" "v1.0.0")
    (path->string proj)
  ) ;git!
  (git! (list "push" "-q" (path->string bare) "refs/tags/v1.0.0")
    (path->string proj)
  ) ;git!
  ;; 同 url 已登记 ref=main，改 ref 要求 --force
  (check-catch 'value-error (get-install url #f "v1.0.0" #f))
  (check (get-install url #f "v1.0.0" #t) => 0)
  (let ((fact (get-lock-entry (get-read-json (get-lock-path)) "demo@liii")))
    (check (json-ref-string fact "ref" "") => "v1.0.0")
    (check (json-ref-string fact "version" "") => "v1.0.0")
  ) ;let
) ;let

;; 9. --ref commit 安装：回退全量 clone + checkout
(let* ((url (string-append "file:///" (path->string bare)))
       (head-commit (get-git-commit proj))
      ) ;
  (check (get-install url #f head-commit #t) => 0)
  (let ((fact (get-lock-entry (get-read-json (get-lock-path)) "demo@liii")))
    (check (json-ref-string fact "commit" "") => head-commit)
    (check (json-ref-string fact "version" "") => head-commit)
  ) ;let
) ;let*

;; 10. --ref @latest：depends 存声明 @latest，lock/安装目录取解析后的真实稳定 tag
(let ((url (string-append "file:///" (path->string bare))))
  ;; 打稳定 + pre-release tag 并推送到 bare
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "tag" "v1.2.0")
    (path->string proj)
  ) ;git!
  (git! (list "push" "-q" (path->string bare) "refs/tags/v1.2.0")
    (path->string proj)
  ) ;git!
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "tag" "v1.10.0")
    (path->string proj)
  ) ;git!
  (git! (list "push" "-q" (path->string bare) "refs/tags/v1.10.0")
    (path->string proj)
  ) ;git!
  (git! (list "-c" "user.email=t@t" "-c" "user.name=t" "tag" "v2.0.0-rc.1")
    (path->string proj)
  ) ;git!
  (git! (list "push" "-q" (path->string bare) "refs/tags/v2.0.0-rc.1")
    (path->string proj)
  ) ;git!
  (check (get-install url #f "@latest" #t) => 0)
  (let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
         (entry (get-depends-entry config "demo@liii"))
         (fact (get-lock-entry (get-read-json (get-lock-path)) "demo@liii"))
         (root (get-package-root "demo@liii"))
        ) ;
    ;; depends 记录声明意图 @latest
    (check (json-ref-string entry "ref" "") => "@latest")
    ;; lock 记录解析事实：最高稳定 tag v1.10.0（排除 v2.0.0-rc.1）
    (check (json-ref-string fact "ref" "") => "v1.10.0")
    (check (json-ref-string fact "version" "") => "v1.10.0")
    (check-true (> (string-length (json-ref-string fact "commit" "")) 0))
    ;; 安装目录为真实 tag 而非字面 @latest
    (check-true (path-file? (path-join root "gfproject.json")))
    (check-false (string-contains? (path->string root) "@latest"))
  ) ;let*
) ;let

(restore-test-home!)
(chdir orig-cwd)
(rmtree (path->string base) #t)

(check-report)

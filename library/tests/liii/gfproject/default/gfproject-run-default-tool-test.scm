;; gfproject-run-default-tool
;; 无参数 gf 使用用户级 pi，显式 gf pi 保持项目级覆盖。
;; 用户级 pi 未注册时，提示安装最新稳定版 gfcode，并指向 gf help。
;;
;; 语法
;; ----
;; (gfproject-run-default-tool command [gf-lib])
;;
;; 参数
;; ----
;; command : string
;; 默认工具名称，目前为 "pi"。
;; gf-lib : string（可选）
;; Goldfish 库根目录。
;;
;; 返回值
;; ----
;; integer
;; 用户级工具退出码；未注册时输出英文安装提示并返回 1。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (liii either)
  (liii sys)
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

(define (write-text path-str text)
  (path-write-text (path path-str) text)
) ;define

(define (child-env home-dir)
  (cons (if (os-windows?) (cons "USERPROFILE" home-dir) (cons "HOME" home-dir))
    (get-environment-variables)
  ) ;cons
) ;define

(define (run-gf args cwd home-dir)
  (run-either (cons (executable) args)
    :cwd    cwd
    :env    (child-env home-dir)
    :stdout 'capture
    :stderr 'capture
  ) ;run-either
) ;define

(define (capture-output args cwd home-dir)
  (let ((res (run-gf args cwd home-dir)))
    (if (either-left? res) (process-error-stdout (to-left res)) (to-right res))
  ) ;let
) ;define

(define (make-pi-project! root value)
  (let ((domain (path->string (path-join root "foo")))
        (lib-dir (path->string (path-join root "foo" "liii" "demo")))
        (config (path->string (path-join root "gfproject.json")))
       ) ;
    (mkdir domain)
    (mkdir (path->string (path-join domain "liii")))
    (mkdir lib-dir)
    (write-text (path->string (path-join lib-dir "cli.scm"))
      (string-append "(define-library (liii demo cli)\n" "  (export main)\n"
        "  (import (scheme base))\n" "  (begin (define (main) (display \"" value
        "\") (newline) 0)))\n"
      ) ;string-append
    ) ;write-text
    (write-text config
      "{\"domains\": {\"foo\": {\"root\": \"foo/\"}}, \"tools\": {}}"
    ) ;write-text
  ) ;let
) ;define

(define (pi-registration root)
  (string-append "{\"tools\": {\"pi\": {\"tools_dir\": \""
    (string-replace root "\\" "\\\\")
    "\", \"tool\": \"foo\", \"organization\": \"liii\","
    " \"module\": \"demo/cli\"}}}"
  ) ;string-append
) ;define

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-default-pi-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
       ) ;base
       (home-dir (path->string (path-join base "home")))
       (gf-dir (path->string (path-join home-dir ".gf")))
       (work-dir (path->string (path-join base "work")))
       (user-ext (path->string (path-join base "ext-user")))
       (local-ext (path->string (path-join base "ext-local")))
       (user-config (path->string (path-join gf-dir "gfproject.json")))
      ) ;
  (mkdir base)
  (mkdir home-dir)
  (mkdir gf-dir)
  (mkdir work-dir)
  (mkdir user-ext)
  (mkdir local-ext)
  (make-pi-project! user-ext "user")
  (make-pi-project! local-ext "local")
  (write-text user-config (pi-registration user-ext))
  (write-text (path->string (path-join work-dir "gfproject.json"))
    (pi-registration local-ext)
  ) ;write-text

  (set-test-home! home-dir)

  ;; 无参数启动选择用户级实现；显式 pi 仍选择当前项目实现。
  (check (string-trim-right (capture-output '() work-dir home-dir)) => "user")
  (check (string-trim-right (capture-output '("pi") work-dir home-dir))
    =>
    "local"
  ) ;check

  ;; 只有项目级注册时，无参数启动不得回退到当前项目。
  (delete-file user-config)
  (let* ((result (run-gf '() work-dir home-dir))
         (err (to-left result))
         (message (process-error-stdout err))
        ) ;
    (check (process-error-code err) => 1)
    (check-true (string-contains? message "not registered in ~/.gf"))
    (check-true (string-contains? message
                  "gf get -g https://github.com/MoganLab/gfcode.git --ref @latest"
                ) ;string-contains?
    ) ;check-true
    (check-true (string-contains? message "For help with gf itself, run:"))
    (check-true (string-contains? message "gf help"))
  ) ;let*
) ;let*

(restore-test-home!)
(chdir orig-cwd)

(check-report)

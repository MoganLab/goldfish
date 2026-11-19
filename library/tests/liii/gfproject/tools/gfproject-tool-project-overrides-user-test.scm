;; 项目层 tools 覆盖用户层同名工具
;; 用户配置指向一个外部项目，当前目录的项目配置指向另一个外部项目；
;; 同名命令应使用项目层的 tools_dir 与模块实现。
;;
;; 语法
;; ----
;; (gfproject-run-tool command)
;;
;; 参数
;; ----
;; command : string
;; 要运行的命令名（本测试为 "demo"）。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 工具 main 的退出码；未找到命令时返回 #f。
;;
;; 说明
;; ----
;; 通过子进程从项目目录启动 bin/gf，验证：
;; - 命令行解析采用项目层 tools 覆盖用户层；
;; - 相对/绝对 tools_dir 以项目层 gfproject.json 所在目录为基准；
;; - 项目内同名工具不会被用户层的实现遮蔽。

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

;; Windows 上 path-home 优先读取 USERPROFILE，POSIX 读取 HOME。
;; 两个变量都指向隔离目录，保证跨平台一致。
(define (set-test-home! dir)
  (putenv "HOME" dir)
  (when (os-windows?) (putenv "USERPROFILE" dir))
) ;define

(define (restore-test-home!)
  (if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
  (when (os-windows?)
    (if orig-profile
      (putenv "USERPROFILE" orig-profile)
      (unsetenv "USERPROFILE")
    ) ;if
  ) ;when
) ;define

;; 捕获子进程的输出；不依赖 sh，Windows 下同样可运行。
(define (capture-output . args)
  (let ((res (run-either (cons (executable) args)
               :stdout 'capture
               :stderr 'capture
             ) ;run-either
       ) ;res
        ) ;
    (if (either-left? res)
      (process-error-stdout (to-left res))
      (to-right res)
    ) ;if
  ) ;let
) ;define

(define (write-text path-str text)
  (path-write-text (path path-str) text)
) ;define

(define (make-tool-project! root value)
  (let ((domain (path->string (path-join root "foo")))
        (lib-dir (path->string (path-join root "foo" "liii" "demo")))
        (config (path->string (path-join root "gfproject.json")))
       ) ;
    (mkdir domain)
    (mkdir (path->string (path-join domain "liii")))
    (mkdir lib-dir)
    (write-text (path->string (path-join lib-dir "cli.scm"))
      (string-append "(define-library (liii demo cli)\n" "  (export main)\n"
        "  (import (scheme base))\n" "  (begin (define (main) (display " value
        ") (newline) 0)))\n"
      ) ;string-append
    ) ;write-text
    (write-text config
      (string-append "{\"domains\": {\"foo\": {\"root\": \"foo/\"}},"
        " \"tools\": {\"demo\": {\"tools_dir\": \""
        (string-replace root "\\\\" "\\\\\\\\")
        "\", \"tool\": \"foo\", \"organization\": \"liii\","
        " \"module\": \"demo/cli\","
        " \"description\": {\"en_US\": \"demo from "
        value
        "\"}}}}"
      ) ;string-append
    ) ;write-text
  ) ;let
) ;define

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-override-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
       ) ;base
       (home-dir (path->string (path-join base "home")))
       (gf-dir (path->string (path-join home-dir ".gf")))
       (work-dir (path->string (path-join base "work")))
       (user-ext (path->string (path-join base "ext-user")))
       (local-ext (path->string (path-join base "ext-local")))
      ) ;
  (mkdir base)
  (mkdir home-dir)
  (mkdir gf-dir)
  (mkdir work-dir)
  (mkdir user-ext)
  (mkdir local-ext)

  ;; 两个外部项目各自提供服务：用户层打印 1，项目层打印 2
  (make-tool-project! user-ext "1")
  (make-tool-project! local-ext "2")

  (write-text (path->string (path-join gf-dir "gfproject.json"))
    (string-append "{\"tools\": {\"demo\": {\"tools_dir\": \""
      (string-replace user-ext "\\\\" "\\\\\\\\")
      "\", \"tool\": \"foo\", \"organization\": \"liii\","
      " \"module\": \"demo/cli\","
      " \"description\": {\"en_US\": \"demo from 1\"}}}}"
    ) ;string-append
  ) ;write-text

  ;; 当前项目把 demo 指向另一个外部项目，项目层应覆盖用户层
  (write-text (path->string (path-join work-dir "gfproject.json"))
    (string-append "{\"tools\": {\"demo\": {\"tools_dir\": \""
      (string-replace local-ext "\\\\" "\\\\\\\\")
      "\", \"tool\": \"foo\", \"organization\": \"liii\","
      " \"module\": \"demo/cli\","
      " \"description\": {\"en_US\": \"demo from 2\"}}}}"
    ) ;string-append
  ) ;write-text

  (set-test-home! home-dir)
  (chdir work-dir)

  ;; 从项目目录启动：项目层配置应覆盖用户层同名 demo
  (check (string-trim-right (capture-output "demo")) => "2")

  ;; 帮助描述也来自项目层
  (check-true (string-contains? (capture-output "help") "demo from 2"))

  (chdir orig-cwd)
) ;let*

(restore-test-home!)
(chdir orig-cwd)

(check-report)

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
  (liii sys)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))

(define orig-cwd (getcwd))

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
       (output (path->string (path-join work-dir "output.txt")))
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

  (putenv "HOME" home-dir)
  (chdir work-dir)

  ;; 从项目目录启动：项目层配置应覆盖用户层同名 demo
  (os-call (string-append "sh -c \"" (executable) " demo > " output " 2>&1\""))
  (check (string-trim-right (path-read-text output)) => "2")

  ;; 帮助描述也来自项目层
  (os-call (string-append "sh -c \"" (executable) " help > " output " 2>&1\""))
  (check-true (string-contains? (path-read-text output) "demo from 2"))

  (chdir orig-cwd)
) ;let*

(if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))
(chdir orig-cwd)

(check-report)

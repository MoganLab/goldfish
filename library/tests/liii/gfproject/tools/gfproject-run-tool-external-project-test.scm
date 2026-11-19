;; gfproject-run-tool 加载外部项目
;; 验证用户层 tools_dir 指向外部项目时，会读取外部项目自己的
;; gfproject.json 并把其 domain 根加入 *load-path*。
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
;; 外部项目：
;;   ext/
;;     gfproject.json   ; domains: base/, foo/（foo depends-on base）
;;     base/liii/demo/base.scm
;;     foo/liii/demo/cli.scm
;; 用户层只登记 pi 之外的 demo 工具，指向 ext 项目根。
;; 工具模块 import 其依赖 domain 的库，验证 domains 由外部项目提供，
;; 未复制到 ~/.gf/gfproject.json。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii gfproject config)
  (liii gfproject tools)
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

(define (write-text path-str text)
  (path-write-text (path path-str) text)
) ;define

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-ext-tool-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
        ) ;base
       (home-dir (path->string (path-join base "home")))
       (gf-dir (path->string (path-join home-dir ".gf")))
       (work-dir (path->string (path-join base "work")))
       (ext-dir (path->string (path-join base "ext")))
       (base-domain (path->string (path-join ext-dir "base")))
       (foo-domain (path->string (path-join ext-dir "foo")))
      ) ;
  ;; 目录骨架
  (mkdir base)
  (mkdir home-dir)
  (mkdir gf-dir)
  (mkdir work-dir)
  (mkdir ext-dir)
  (mkdir base-domain)
  (mkdir foo-domain)
  (mkdir (path->string (path-join base-domain "liii")))
  (mkdir (path->string (path-join base-domain "liii" "demo")))
  (mkdir (path->string (path-join foo-domain "liii")))
  (mkdir (path->string (path-join foo-domain "liii" "demo")))

  ;; 外部项目自身配置：domains 相对 ext 根；工具 cha 位于 foo domain
  (write-text (path->string (path-join ext-dir "gfproject.json"))
    (string-append
      "{\"domains\": {\"base\": {\"root\": \"base/\"},"
      " \"foo\": {\"root\": \"foo/\", \"depends-on\": [\"base\"]}},"
      " \"tools\": {\"demo\": {\"tools_dir\": \".\", \"tool\": \"foo\","
      " \"organization\": \"liii\", \"module\": \"demo/cli\"}}}"
    ) ;string-append
  ) ;write-text

  ;; 依赖 domain 的库
  (write-text (path->string (path-join base-domain "liii" "demo" "base.scm"))
    (string-append "(define-library (liii demo base)\n"
      "  (export demo-base-value)\n"
      "  (import (scheme base))\n"
      "  (begin (define demo-base-value 42)))\n"
    ) ;string-append
  ) ;write-text

  ;; 工具模块：import 依赖 domain 的库，main 返回 0
  (write-text (path->string (path-join foo-domain "liii" "demo" "cli.scm"))
    (string-append "(define-library (liii demo cli)\n"
      "  (export main)\n"
      "  (import (scheme base) (liii demo base))\n"
      "  (begin (define (main) (if (= demo-base-value 42) 0 1))))\n"
    ) ;string-append
  ) ;write-text

  ;; 用户层只登记工具，不复制 ext 的 domains
  (write-text (path->string (path-join gf-dir "gfproject.json"))
    (string-append
      "{\"tools\": {\"demo\": {\"tools_dir\": \""
      (string-replace ext-dir "\\\\" "\\\\\\\\")
      "\", \"tool\": \"foo\", \"organization\": \"liii\","
      " \"module\": \"demo/cli\"}}}"
    ) ;string-append
  ) ;write-text

  (set-test-home! home-dir)
  (chdir work-dir)

  ;; 外部项目根应能以工具根向上定位
  (check (gfproject-find-project-root foo-domain) => ext-dir)

  ;; 运行工具：成功说明 base domain 已由外部项目配置注入 load-path
  (check (gfproject-run-tool "demo") => 0)

  (chdir orig-cwd)
  (delete-file (path->string (path-join gf-dir "gfproject.json")))
) ;let*

(restore-test-home!)
(chdir orig-cwd)

(check-report)

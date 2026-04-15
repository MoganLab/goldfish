;; 添加 tools/help 到 load path，以便导入 (liii goldhelp)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path*
  (cons "tools/help" *load-path*)
) ;set!

(import (liii check)
  (liii goldhelp)
  (liii json)
  (liii os)
  (liii path)
  (liii string)
  (liii sys)
) ;import

(check-set-mode! 'report-failed)

(define (run-shell-command command)
  (os-call (string-append "sh -c \"" command "\"")
  ) ;os-call
) ;define

(define (cleanup-gfproject-fixture base-root)
  (let ((config-path (path-join base-root "gfproject.json")
        ) ;config-path
        (output-path (path-join base-root "version.log")
        ) ;output-path
       ) ;
    (path-unlink output-path #t)
    (path-unlink config-path #t)
    (if (path-dir? base-root)
      (path-rmdir base-root)
      #f
    ) ;if
  ) ;let
) ;define

(when (not (os-windows?))
  (let* ((base-root (path-join (path-temp-dir)
                      (string-append "goldhelp-gfproject-"
                        (number->string (getpid))
                      ) ;string-append
                    ) ;path-join
         ) ;base-root
         (config-path (path-join base-root "gfproject.json")
         ) ;config-path
         (output-path (path-join base-root "version.log")
         ) ;output-path
         (old-cwd (getcwd))
         (merge-fixture "{\n  \"tools\": {\n    \"test\": {\n      \"description\": {\n        \"zh_CN\": \"运行测试（本地覆盖）\"\n      }\n    }\n  }\n}\n"
         ) ;merge-fixture
         (broken-version-fixture "{\n  \"tools\": {\n    \"version\": {\n      \"module\": \"goldversion_missing\"\n    }\n  }\n}\n"
         ) ;broken-version-fixture
        ) ;
    (cleanup-gfproject-fixture base-root)
    (mkdir (path->string base-root))
    (dynamic-wind (lambda () #t)
      (lambda ()
        (chdir (path->string base-root))

        ;; 字段级深度合并：本地只补 description.zh_CN，不应丢失内置 organization/module/en_US
        (path-write-text config-path
          merge-fixture
        ) ;path-write-text
        (let* ((config (load-gfproject))
               (tools (json-ref config "tools"))
               (test-tool (json-ref tools "test"))
               (test-desc (json-ref test-tool "description")
               ) ;test-desc
              ) ;
          (check (json-ref test-tool "organization")
            =>
            "liii"
          ) ;check
          (check (json-ref test-tool "module")
            =>
            "goldtest"
          ) ;check
          (check (json-ref test-desc "en_US")
            =>
            "Run tests (all *-test.scm files under tests/)"
          ) ;check
          (check (json-ref test-desc "zh_CN")
            =>
            "运行测试（本地覆盖）"
          ) ;check
        ) ;let*

        ;; 单命令回退：本地把 version.module 覆盖成错误值时，gf version 仍应回退到 lib 实现
        (path-write-text config-path
          broken-version-fixture
        ) ;path-write-text
        (path-unlink output-path #t)
        (run-shell-command (string-append (executable)
                             " version > "
                             (path->string output-path)
                             " 2>&1"
                           ) ;string-append
        ) ;run-shell-command
        (check-true (string-contains? (path-read-text output-path)
                      "Goldfish Scheme"
                    ) ;string-contains?
        ) ;check-true
      ) ;lambda
      (lambda ()
        (chdir old-cwd)
        (cleanup-gfproject-fixture base-root)
      ) ;lambda
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)

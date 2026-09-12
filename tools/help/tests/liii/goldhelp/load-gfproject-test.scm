;; 添加 tools/help 到 load path，以便导入 (liii goldhelp)
;; 注意：假设运行测试时工作目录是项目根目录
(set! *load-path* (cons "tools/help" *load-path*))

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
  (os-call (string-append "sh -c \"" command "\""))
) ;define

(define (cleanup-gfproject-fixture base-root)
  (let ((config-path (path-join base-root "gfproject.scm"))
        (output-path (path-join base-root "version.log"))
       ) ;
    (path-unlink output-path #t)
    (path-unlink config-path #t)
    (if (path-dir? base-root) (path-rmdir base-root) #f)
  ) ;let
) ;define

(when (not (os-windows?))
  (let* ((base-root (path-join (path-temp-dir)
                      (string-append "goldhelp-gfproject-" (number->string (getpid)))
                    ) ;path-join
         ) ;base-root
         (config-path (path-join base-root "gfproject.scm"))
         (output-path (path-join base-root "version.log"))
         (old-cwd (getcwd))
         (merge-fixture "(gfproject (tools (test (description (zh_CN \"运行测试（本地覆盖）\")))))\n"
         ) ;merge-fixture
         (broken-version-fixture "(gfproject (tools (version (module \"goldversion_missing\"))))\n"
         ) ;broken-version-fixture
        ) ;
    (cleanup-gfproject-fixture base-root)
    (mkdir (path->string base-root))
    (dynamic-wind (lambda () #t)
      (lambda ()
        (chdir (path->string base-root))

        ;; 字段级深度合并：本地只补 description.zh_CN，不应丢失内置 organization/module/en_US
        ;; load-gfproject 返回 SEXP 视图的 tools alist（symbol 键），直接 assq 提取。
        (path-write-text config-path merge-fixture)
        (let* ((config (load-gfproject))
               (test-tool (cdr (assq 'test config)))
               (test-desc (cdr (assq 'description test-tool)))
              ) ;
          (check (cadr (assq 'organization test-tool)) => 'liii)
          (check (cadr (assq 'module test-tool)) => 'goldtest)
          (check (cadr (assq 'en_US test-desc))
            =>
            "Run tests (all *-test.scm files under tests/)"
          ) ;check
          (check (cadr (assq 'zh_CN test-desc)) => "运行测试（本地覆盖）")
        ) ;let*

        ;; 单命令回退：本地把 version.module 覆盖成错误值时，gf version 仍应回退到 lib 实现
        (path-write-text config-path broken-version-fixture)
        (path-unlink output-path #t)
        (run-shell-command (string-append (executable) " version > " (path->string output-path) " 2>&1")
        ) ;run-shell-command
        (check-true (string-contains? (path-read-text output-path) "Goldfish Scheme"))
      ) ;lambda
      (lambda () (chdir old-cwd) (cleanup-gfproject-fixture base-root))
    ) ;dynamic-wind
  ) ;let*
) ;when

(check-report)

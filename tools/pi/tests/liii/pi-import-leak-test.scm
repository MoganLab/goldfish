;; 回归测试：只导入 (liii check) 与 (liii pi agent queue)、不显式导入 (scheme process-context)
;; 时，调用 get-environment-variable 必须得到结构化的 unbound-variable 错误，
;; 而不是进程段错误。
;;
;; 历史缺陷：未捕获的 unbound-variable 在打印错误时会扫描 *load-path* 寻找提示，
;; 旧实现用 s7 reader 读取源码文件；一旦扫描到当前 reader 无法解析的文件（例如
;; 未导入 (liii raw-string) 时的 #"" 原始字符串、或未闭合字符串），错误处理路径
;; 会再次抛错，s7 在构造 stacktrace 时段错误。提示扫描现在只做文本层面的
;; s-expression 扫描，必须能跳过坏文件并继续给出提示。
(import (liii check)
        (liii either)
        (liii os)
        (liii path)
        (liii string)
        (liii subprocess)
        (liii sys)
        )
(check-set-mode! 'report-failed)

(define (pi-import-leak-cleanup base)
  (define (drop-file path)
    (when (path-file? path)
      (delete-file (path->string path))
    ) ;when
  ) ;define
  (define (drop-dir path)
    (when (path-dir? path)
      (path-rmdir (path->string path))
    ) ;when
  ) ;define
  (drop-file (path-join base "leak-script.scm"))
  (drop-file (path-join base "libroot" "bad" "broken.scm"))
  (drop-file (path-join base "libroot" "bad" "raw.scm"))
  (drop-file (path-join base "libroot" "good" "defs.scm"))
  (drop-dir (path-join base "libroot" "bad"))
  (drop-dir (path-join base "libroot" "good"))
  (drop-dir (path-join base "libroot"))
  (drop-dir base)
) ;define

(define (pi-import-leak-load-path-args)
  ;; 优先用 bin/gf 所在仓库的 tools/pi；找不到时退回当前目录。
  (let* ((repo-root (path-parent (path-parent (path (executable)))))
         (tools-dir (path-join repo-root "tools" "pi"))
        ) ;
    (if (path-dir? tools-dir)
      (list "-I" (path->string tools-dir))
      (list "-I" ".")
    ) ;if
  ) ;let*
) ;define

(define (pi-import-leak-run-script script-path)
  (run-either (append (list (executable))
                 (pi-import-leak-load-path-args)
                 (list "-m" "r7rs" (path->string script-path))
               ) ;append
    :stdout 'capture
    :stderr 'stdout
    :timeout 120
  ) ;run-either
) ;define

(let* ((base (path-join (path-temp-dir)
               (string-append "gf-pi-import-leak-" (number->string (getpid)))
             ) ;path-join
       ) ;base
       (libroot (path-join base "libroot"))
       (bad-dir (path-join libroot "bad"))
       (good-dir (path-join libroot "good"))
       (script-path (path-join base "leak-script.scm"))
      ) ;
  (pi-import-leak-cleanup base)
  (mkdir (path->string base))
  (mkdir (path->string libroot))
  (mkdir (path->string bad-dir))
  (mkdir (path->string good-dir))
  ;; 坏文件：未闭合字符串。提示扫描必须跳过它，而不是让整个进程崩溃。
  (path-write-text (path-join bad-dir "broken.scm")
    "(define-library (fixture broken)\n  (export nothing-here)\n  (begin\n    (define value \"unterminated\n  )\n)\n"
  ) ;path-write-text
  ;; 原始字符串文件：(liii raw-string) 未导入时 s7 reader 无法读取这类文件。
  (path-write-text (path-join bad-dir "raw.scm")
    "(define-library (fixture raw)\n  (export raw-only)\n  (begin\n    (define value (&-#\"\"\n      (char=? #\\a #\\()\n      \"\"))\n  )\n)\n"
  ) ;path-write-text
  ;; 正常库：导出 get-environment-variable，用于验证提示仍能给出可用的导入建议。
  (path-write-text (path-join good-dir "defs.scm")
    "(define-library (fixture process)\n  (export get-environment-variable)\n  (begin)\n)\n"
  ) ;path-write-text
  (path-write-text script-path
    (string-append "(import (liii check) (liii pi agent queue))\n"
      "(set! *load-path* (append (list \""
      (string-replace (path->string libroot) "\\" "/")
      "\") *load-path*))\n"
      "(display (get-environment-variable \"PATH\"))\n"
    ) ;string-append
  ) ;path-write-text

  (dynamic-wind
    (lambda () #f)
    (lambda ()
      ;; 进程内：语义必须是 unbound-variable 结构化错误，不能静默成功
      (check-catch 'unbound-variable (get-environment-variable "PATH"))
      ;; 子进程：未捕获错误会触发提示扫描，必须正常报错退出且给出提示
      (let ((result (pi-import-leak-run-script script-path)))
        (check (either-left? result) => #t)
        (let* ((process-error (to-left result))
               (code (process-error-code process-error))
               (output (process-error-stdout process-error))
              ) ;
          (check (number? code) => #t)
        (check-true (string-contains? output "unbound variable get-environment-variable"))
        (check-true (string-contains? output "Hint:"))
          (check-true (string-contains? output "(fixture process)"))
        ) ;let*
      ) ;let
    ) ;lambda
    (lambda () (pi-import-leak-cleanup base))
  ) ;dynamic-wind
) ;let*

(check-report)

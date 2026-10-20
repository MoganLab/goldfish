;; 提示（hint）扫描器的健壮性测试。
;;
;; 未捕获的 unbound-variable 在打印错误时会扫描 *load-path* 寻找
;; “该函数在哪个库导出”的提示。旧实现用 s7 reader 读取源码文件：
;;  1. 扫描到当前 reader 无法解析的文件时会再次抛错；
;;  2. 错误处理路径里的二次抛错会让 s7 在构造 stacktrace 时段错误。
;; 现在改为纯文本 s-expression 扫描，本测试确保：
;;  - 坏文件（未闭合字符串、括号不平衡、嵌套过深）被跳过；
;;  - 原始字符串等 goldfish reader 扩展不会破坏扫描；
;;  - 注释、#; datum comment、rename 导出都按语义处理；
;;  - CLI 在坏文件存在时仍然给出提示并以普通错误码退出（不崩溃）。
(import (liii check)
        (liii either)
        (liii os)
        (liii path)
        (liii string)
        (liii subprocess)
        (liii sys))

(check-set-mode! 'report-failed)

(define (library-hint-cleanup base)
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
  (for-each
    (lambda (file)
      (drop-file (path-join base "libroot" "bad" file))
      (drop-file (path-join base "libroot" "good" file))
    ) ;lambda
    '("broken.scm" "raw.scm" "deep.scm" "odd.scm" "quoted.scm" "unbalanced.scm")
  ) ;for-each
  (drop-file (path-join base "hint-script.scm"))
  (drop-dir (path-join base "libroot" "bad"))
  (drop-dir (path-join base "libroot" "good"))
  (drop-dir (path-join base "libroot"))
  (drop-dir base)
) ;define

(define (library-hint-run-script script-path)
  (run-either (list (executable) "-m" "r7rs" (path->string script-path))
    :stdout 'capture
    :stderr 'stdout
    :timeout 120
  ) ;run-either
) ;define

(let* ((base (path-join (path-temp-dir)
               (string-append "gf-library-hint-" (number->string (getpid)))
             ) ;path-join
       ) ;base
       (libroot (path-join base "libroot"))
       (bad-dir (path-join libroot "bad"))
       (good-dir (path-join libroot "good"))
       (script-path (path-join base "hint-script.scm"))
      ) ;
  (library-hint-cleanup base)
  (mkdir (path->string base))
  (mkdir (path->string libroot))
  (mkdir (path->string bad-dir))
  (mkdir (path->string good-dir))
  (path-write-text (path-join bad-dir "broken.scm")
    "(define-library (fixture broken)\n  (export nothing-here)\n  (begin\n    (define value \"unterminated\n  )\n)\n"
  ) ;path-write-text
  (path-write-text (path-join bad-dir "raw.scm")
    "(define-library (fixture raw)\n  (export raw-only)\n  (begin\n    (define value (&-#\"\"\n      (char=? #\\a #\\()\n      \"\"))\n  )\n)\n"
  ) ;path-write-text
  (path-write-text (path-join bad-dir "unbalanced.scm")
    "(define-library (fixture unbalanced)\n  (export broken-only)\n  (begin)\n"
  ) ;path-write-text
  (path-write-text (path-join bad-dir "deep.scm")
    (string-append "(define-library (fixture deep)\n  (export deep-only)\n  (begin\n    (define value '"
      (make-string 400 #\()
      (make-string 400 #\))
      ")\n  )\n)\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text (path-join good-dir "odd.scm")
    (string-append "#| block comment with ( parens ) and ; semicolon |#\n"
      "(define-library (fixture odd)\n"
      "  (import (scheme base))\n"
      "  #;(export not-this)\n"
      "  (export (rename hidden hinted-fn) plain-fn)\n"
      "  (begin\n"
      "    (define hidden 1)\n"
      "    (define plain-fn 2)\n"
      "    (define text \"string with ( parens ) and ; semicolon\")\n"
      "    (define open-char #\\()\n"
      "  )\n"
      ")\n"
    ) ;string-append
  ) ;path-write-text
  (path-write-text (path-join good-dir "quoted.scm")
    "'(define-library (fixture quoted) (export quoted-only))\n"
  ) ;path-write-text
  (path-write-text script-path
    (string-append "(import (scheme base))\n"
      "(set! *load-path* (append (list "
      (object->string (path-as-posix libroot))
      ") *load-path*))\n"
      "(display (hinted-fn 1))\n"
    ) ;string-append
  ) ;path-write-text

  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (let ((old-load-path *load-path*))
        (set! *load-path* (list (path->string libroot)))
        (check (g_function-libraries "hinted-fn") => '((fixture odd)))
        (check (g_function-libraries "plain-fn") => '((fixture odd)))
        (check (g_function-libraries "not-this") => '())
        (check (g_function-libraries "quoted-only") => '())
        (check (g_function-libraries "nothing-here") => '())
        (check (g_function-libraries "raw-only") => '((fixture raw)))
        (set! *load-path* old-load-path)
      ) ;let
      ;; CLI：错误打印时的提示扫描必须跳过坏文件并给出提示
      (let ((result (library-hint-run-script script-path)))
        (check (either-left? result) => #t)
        (let* ((process-error (to-left result))
               (code (process-error-code process-error))
               (output (process-error-stdout process-error))
              ) ;
          (check (number? code) => #t)
          (check-true (string-contains? output "unbound variable hinted-fn"))
          (check-true (string-contains? output "Hint:"))
          (check-true (string-contains? output "(fixture odd)"))
        ) ;let*
      ) ;let
    ) ;lambda
    (lambda () (library-hint-cleanup base))
  ) ;dynamic-wind
) ;let*

(check-report)

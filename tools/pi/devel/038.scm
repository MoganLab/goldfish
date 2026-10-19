(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [038] 验收脚本：get-environment-variable 段错误调查
;; 逐条对应 tools/pi/devel/038.md 的验收点：
;;   1) 门禁：构建 / fmt --check / check
;;   2) 两个新增测试（library-hint-test / pi-import-leak-test）
;;   3) 显式导入路径对照（无回归）
;;   4) 子进程未捕获错误三场景：退出码 255、含 Hint、不段错误
;;      - get-environment-variable（原始现场）
;;      - string-prefix?（027 同类线索）
;;      - process-spawn（附加线索）

(define %tmp-files '())

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-038-" tag ".txt")))
    (set! %tmp-files (cons tmp %tmp-files))
    (let* ((ret (%run-shell (string-append cmd " > " tmp " 2>&1") timeout))
           (out
             (if (and (file-exists? tmp) (path-file? (path tmp))) (path-read-text tmp) "")
           ) ;out
          ) ;
      (cons ret out)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (let* ((res (%capture tag cmd timeout)) (rc (car res)))
    (unless (zero? rc)
      (display (string-append "!!! " tag " exited " (number->string rc) "\n" (cdr res))
      ) ;display
    ) ;unless
    rc
  ) ;let*
) ;define

(define (%cleanup!)
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (set! %tmp-files '())
) ;define

;; 子进程场景：脚本只导入 (liii pi)（及场景需要的库），不显式导入目标库，
;; 未捕获的 unbound-variable 必须触发提示扫描并以普通错误码退出。

(define (%write-scenario dir name body)
  (let ((script (path-join dir (string-append name ".scm"))))
    (path-write-text script body)
    (path->string script)
  ) ;let
) ;define

(define (%run-scenario dir name body)
  (let* ((script (%write-scenario dir name body))
         (cmd (string-append "timeout 60 bin/gf -I tools/pi -m r7rs " script))
        ) ;
    (%capture (string-append "scn-" name) cmd 90)
  ) ;let*
) ;define

(define (%check-scenario dir name body expected-lib)
  (let* ((res (%run-scenario dir name body)) (rc (car res)) (out (cdr res)))
    (unless (= rc 255)
      (display (string-append "!!! scenario " name " exited " (number->string rc) "\n" out)
      ) ;display
    ) ;unless
    (check rc => 255)
    (check-true (string-contains? out "unbound variable"))
    (check-true (string-contains? out "Hint:"))
    (check-true (string-contains? out expected-lib))
    (check (string-contains? out "Segmentation fault") => #f)
  ) ;let*
) ;define

(let* ((base
         (path-join (path-temp-dir)
           (string-append "gf-accept-038-" (number->string (getpid)))
         ) ;path-join
       ) ;base
      ) ;
  (when (path-dir? base)
    (path-rmdir (path->string base))
  ) ;when
  (mkdir (path->string base))

  (dynamic-wind (lambda () (%cleanup!))
    (lambda ()
      (display "=== 验收点 1: 门禁检查（构建 / fmt / check） ===\n")
      (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
      (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 120) => 0)
      (check (%ok? "check" "timeout 120 bin/gf check" 120) => 0)
      (check (%ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 120) => 0)

      (display "=== 验收点 2: 两个新增防回归测试 ===\n")
      (check (%ok? "t-hint" "timeout 120 bin/gf test tests/scheme/library-hint-test.scm" 120)
        =>
        0
      ) ;check
      (check (%ok? "t-leak"
               "timeout 120 bin/gf test tools/pi/tests/liii/pi-import-leak-test.scm"
               120
             ) ;%ok?
        =>
        0
      ) ;check

      (display "=== 验收点 3: 显式导入路径对照（无回归） ===\n")
      (check (%ok? "t-env"
               "timeout 120 bin/gf test tests/scheme/process-context/get-environment-variable-test.scm"
               120
             ) ;%ok?
        =>
        0
      ) ;check
      (check (%ok? "t-pc"
               "timeout 120 bin/gf test tests/scheme/process-context-test.scm"
               120
             ) ;%ok?
        =>
        0
      ) ;check
      (check (%ok? "t-spawn"
               "timeout 120 bin/gf test tests/liii/subprocess/process-spawn-test.scm"
               120
             ) ;%ok?
        =>
        0
      ) ;check

      (display "=== 验收点 4: 子进程未捕获错误三场景（255 + Hint + 不段错误） ===\n"
      ) ;display
      (%check-scenario base
        "scn-env"
        (string-append "(import (scheme base) (liii pi))\n"
          "(display (get-environment-variable \"PATH\"))\n"
        ) ;string-append
        "(scheme process-context)"
      ) ;%check-scenario
      (%check-scenario base
        "scn-prefix"
        (string-append "(import (scheme base) (liii string) (liii pi))\n"
          "(display (string-prefix? \"ab\" \"abc\"))\n"
        ) ;string-append
        "(srfi srfi-13)"
      ) ;%check-scenario
      (%check-scenario base
        "scn-spawn"
        (string-append "(import (scheme base) (liii pi))\n"
          "(display (process-spawn \"true\"))\n"
        ) ;string-append
        "(liii subprocess)"
      ) ;%check-scenario
    ) ;lambda
    (lambda () (%cleanup!) (when (path-dir? base) (path-rmdir (path->string base))))
  ) ;dynamic-wind
) ;let*

(check-report)

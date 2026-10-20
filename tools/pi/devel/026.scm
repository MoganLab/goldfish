(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
) ;import

(check-set-mode! 'report-failed)

;; [026] 缺陷修复·Windows bash 工具挂死 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: pi-find-bash-path 探测函数断言 ===\n")

;; 1.1 默认平台探测
(let ((path (pi-find-bash-path)))
  (check-true (string? path))
  (check-true (> (string-length path) 0))
) ;let

;; 1.2 模拟 posix 平台探测
(let ((posix-path (pi-find-bash-path 'posix)))
  (check-true (string? posix-path))
  (check-true (> (string-length posix-path) 0))
) ;let

;; 1.3 模拟 windows 平台探测
(let ((win-path (pi-find-bash-path 'windows)))
  (check-true (string? win-path))
  (check-true (> (string-length win-path) 0))
) ;let

(display "=== 验收点 2: 独立单元测试文件全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-execute-bash-test.scm" 30)
  =>
  0
) ;check

(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-find-bash-path-test.scm" 30)
  =>
  0
) ;check

(display "=== 验收点 3: bash 工具 print 模式真实冒烟 ===\n")
(let* ((tmp-out "tmp-026-smoke-print.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --tools bash \"用 bash 工具运行命令 echo win-bash-ok\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              180
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "win-bash-ok"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 4: bash 工具 JSON 模式真实冒烟 ===\n")
(let* ((tmp-out "tmp-026-smoke-json.txt")
       (ret (%run-shell (string-append "bin/gf pi --mode json --tools bash \"用 bash 工具运行命令 echo win-bash-ok\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              180
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "tool_execution_start"))
  (check-true (string-contains? out "tool_execution_end"))
  (check-true (string-contains? out "\"isError\":false"))
  (check-true (string-contains? out "win-bash-ok"))
  (check-false (string-contains? out "Command timed out"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 5: 门禁检查通过 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
(check (%run-shell "git diff --exit-code tools/pi/liii/ tools/pi/tests/" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)

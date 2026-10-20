(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi args)
  (liii pi tools)
  (liii pi config)
) ;import

(check-set-mode! 'report-failed)

;; [024] --exclude-tools 支持 验收脚本

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(display "=== 验收点 1: CLI 参数解析支持 --exclude-tools ===\n")
;; 1.1 显式传递参数
(let ((res (parse-pi-args '("--exclude-tools" "bash,write,edit"))))
  (check (pi-args-ref res 'exclude-tools) => "bash,write,edit")
) ;let

;; 1.2 默认未指定时为 #f
(let ((res (parse-pi-args '())))
  (check (pi-args-ref res 'exclude-tools) => #f)
) ;let

(display "=== 验收点 2: 工具排除过滤逻辑 (exclude-tools-from-list) ===\n"
) ;display
;; 2.1 基础排除与空规格容错
(check (exclude-tools-from-list '("read" "bash" "edit" "write") #f)
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "")
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "bash,write,edit")
  =>
  '("read")
) ;check

;; 2.2 首尾空白与逗号修剪
(check (exclude-tools-from-list '("read" "bash" "edit" "write") " bash , edit ")
  =>
  '("read" "write")
) ;check

;; 2.3 包含未知或未启用工具时安全忽略
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "foo,bash")
  =>
  '("read" "edit" "write")
) ;check

;; 2.4 全部排除返回空列表
(check (exclude-tools-from-list '("read" "bash") "read,bash") => '())

(display "=== 验收点 3: 白名单与黑名单共存时的优先级 (排除优先) ===\n"
) ;display
(let ((args-ex (parse-pi-args '("--exclude-tools" "bash,write,edit")))
      (args-both (parse-pi-args '("--tools" "read,bash,edit" "--exclude-tools"
                                  "bash"))
      ) ;args-both
      (args-unknown (parse-pi-args '("--exclude-tools" "foo,bash")))
      (args-spaces (parse-pi-args '("--exclude-tools" " bash , edit ")))
      (settings '((default-tools "read" "bash" "edit" "write")))
     ) ;
  ;; 3.1 仅指定 --exclude-tools：从默认工具集中排除
  (check (resolve-active-tools args-ex settings) => '("read"))
  ;; 3.2 --tools 与 --exclude-tools 共存：排除优先
  (check (resolve-active-tools args-both settings) => '("read" "edit"))
  ;; 3.3 未知工具容错
  (check (resolve-active-tools args-unknown settings) => '("read"
                                                           "edit"
                                                           "write"))
  ;; 3.4 空白容错
  (check (resolve-active-tools args-spaces settings) => '("read" "write"))
) ;let

(display "=== 验收点 4: 帮助文本包含 --exclude-tools ===\n")
(let* ((tmp-out "tmp-024-help.txt")
       (ret (%run-shell (string-append "bin/gf pi --help > " tmp-out " 2>&1") 10))
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "--exclude-tools <list>"))
  (check-true (string-contains? out "Comma-separated list of tools to exclude"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 5: 单元测试全量通过 ===\n")
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-args-test.scm" 30) => 0)
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-tools-basic-test.scm" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi-config-test.scm" 30)
  =>
  0
) ;check

(display "=== 验收点 6: 真实冒烟 ===\n")
;; 6.1 从默认工具中排除 bash,write,edit，验证只留 read
(let* ((tmp-out "tmp-024-smoke-1.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --exclude-tools bash,write,edit \"只输出工具名称列表，逗号分隔，不要解释\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              60
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "read"))
  (check-false (string-contains? out "bash"))
  (check-false (string-contains? out "write"))
  (check-false (string-contains? out "edit"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 6.2 --tools 与 --exclude-tools 组合，验证排除优先
(let* ((tmp-out "tmp-024-smoke-2.txt")
       (ret (%run-shell (string-append "bin/gf pi --print --tools read,bash,edit --exclude-tools bash \"只输出工具名称列表，逗号分隔，不要解释\" > "
                          tmp-out
                          " 2>&1"
                        ) ;string-append
              60
            ) ;%run-shell
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "read"))
  (check-true (string-contains? out "edit"))
  (check-false (string-contains? out "bash"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 7: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
(check (%run-shell "git diff --exit-code tools/pi/liii/ tools/pi/tests/" 30)
  =>
  0
) ;check
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)

(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [018] --list-models 支持 验收脚本

(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))
(eval '(import (liii pi args)) (rootlet))
(eval '(import (liii pi models)) (rootlet))
(eval '(import (liii pi models list)) (rootlet))
(eval '(import (liii pi help)) (rootlet))

(define (%read-file-content path)
  (call-with-input-file path
    (lambda (p)
      (let loop
        ((acc '()))
        (let ((l (read-line p)))
          (if (eof-object? l) (string-join (reverse acc) "\n") (loop (cons l acc)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; ============================================================================
;; 验收点 1: 函数导出与接口存在性
;; ============================================================================

(check-true (procedure? pi-format-token-count))
(check-true (procedure? pi-list-models))
(check-true (procedure? pi-format-models-table))

;; 1.1 token count 格式化对齐
(check (pi-format-token-count 1000000) => "1M")
(check (pi-format-token-count 2000000) => "2M")
(check (pi-format-token-count 1500000) => "1.5M")
(check (pi-format-token-count 128000) => "128K")
(check (pi-format-token-count 65536) => "65.5K")
(check (pi-format-token-count 131072) => "131.1K")
(check (pi-format-token-count 8192) => "8.2K")
(check (pi-format-token-count 500) => "500")

;; ============================================================================
;; 验收点 2: 命令行参数解析
;; ============================================================================

(let ((res1 (parse-pi-args '("--list-models")))
      (res2 (parse-pi-args '("--list-models" "kimi")))
      (res3 (parse-pi-args '("--list-models" "--provider" "google")))
      (res4 (parse-pi-args '("--provider" "google" "--list-models")))
      (res5 (parse-pi-args '("--list-models" "@doc.txt")))
     ) ;
  (check (pi-args-ref res1 'list-models) => #t)
  (check (pi-args-ref res2 'list-models) => "kimi")
  (check (pi-args-ref res3 'list-models) => #t)
  (check (pi-args-ref res3 'provider) => "google")
  (check (pi-args-ref res4 'list-models) => #t)
  (check (pi-args-ref res4 'provider) => "google")
  (check (pi-args-ref res5 'list-models) => #t)
  (check (pi-args-ref res5 'file-args) => '("doc.txt"))
) ;let

;; ============================================================================
;; 验收点 3: --help 包含 --list-models 说明
;; ============================================================================

(check-true (string-contains? PI_HELP_TEXT "--list-models [search]"))

;; ============================================================================
;; 验收点 4: 运行 bin/gf pi --list-models 退出码 0 且包含模型表格表头与数据
;; ============================================================================

(let* ((tmp-out "/tmp/pi-018-list-all.txt")
       (code (run (string-append "sh -c 'bin/gf pi --list-models > " tmp-out "'")
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "provider"))
    (check-true (string-contains? out "model"))
    (check-true (string-contains? out "context"))
    (check-true (string-contains? out "max-out"))
    (check-true (string-contains? out "thinking"))
    (check-true (string-contains? out "images"))
    (check-true (string-contains? out "gemini-2.5-flash"))
  ) ;let
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; ============================================================================
;; 验收点 5: 运行 bin/gf pi --provider kimi-coding --list-models 过滤生效
;; ============================================================================

(let* ((tmp-out "/tmp/pi-018-list-kimi.txt")
       (code (run (string-append "sh -c 'bin/gf pi --provider kimi-coding --list-models > "
                    tmp-out
                    "'"
                  ) ;string-append
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "kimi-coding"))
    (check-true (string-contains? out "k3"))
    (check-false (string-contains? out "gemini-"))
  ) ;let
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; ============================================================================
;; 验收点 6: 未知 provider 报错退出码 1
;; ============================================================================

(let* ((tmp-err "/tmp/pi-018-err.txt")
       (code (run (string-append "sh -c 'bin/gf pi --provider unknown-prov-xyz-018 --list-models 2> "
                    tmp-err
                    "'"
                  ) ;string-append
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 1)
  (let ((err (%read-file-content tmp-err)))
    (check-true (string-contains? err "Unknown provider"))
  ) ;let
  (if (file-exists? tmp-err) (delete-file tmp-err))
) ;let*

;; ============================================================================
;; 验收点 7: 可选模糊搜索词过滤测试
;; ============================================================================

(let* ((tmp-out "/tmp/pi-018-search.txt")
       (code (run (string-append "sh -c 'bin/gf pi --list-models flash > " tmp-out "'")
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "gemini-2.5-flash"))
    (check-false (string-contains? out "gemini-2.5-pro"))
  ) ;let
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; ============================================================================
;; 验收点 8: 全量单元测试通过
;; ============================================================================

(check (run "bin/gf test tools/pi/tests/liii/pi/models/list/pi-list-models-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi/models/list/pi-format-models-table-test.scm"
         :timeout 30
       ) ;run
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

;; ============================================================================
;; 验收点 9: 门禁检查通过
;; ============================================================================

(check (run "timeout 600 xmake b goldfish" :timeout 60) => 0)
(check (run "bin/gf fmt" :timeout 30) => 0)
(check (run "bin/gf check" :timeout 30) => 0)

(check-report)

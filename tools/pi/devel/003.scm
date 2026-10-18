(import (scheme base)
  (scheme file)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [003] 核心库迁移·模块名统一 验收脚本
;;
;; 验收依据：PI_SPEC.md 架构演进节与 tools/pi/devel/003.md 验收点：
;; 1. 目标子库文件是否存在且可 import（(liii pi types/gemini/anthropic/auth/models/retry/agent/tools/config/prompt/session/args/modes)）
;;    缺失的子库如实记为失败项，不许降低标准。
;; 2. 二级子库为导出层（已存在的二级子库包含 export 声明）
;; 3. 单文件不超 300 行（bin/gf check 通过且 tools/pi/ 下各源文件行数 <= 300）
;; 4. CLI 侧无 goldpi* 残留（文件重命名完成，旧文件不存在）
;; 5. gfproject.json 指向 (liii pi-cli)
;; 6. bin/gf test 全过（核心测试均以退出码 0 通过）
;; 7. bin/gf pi --help 可用

;; 辅助函数：检查子库定义是否有效

(define (sublibrary-ready? file-path expected-lib)
  (and (file-exists? file-path)
    (guard (e (else #f))
      (call-with-input-file file-path
        (lambda (port)
          (let loop
            ((form (read port)))
            (cond ((eof-object? form) #f)
                  ((and (pair? form)
                     (eq? (car form) 'define-library)
                     (pair? (cdr form))
                     (equal? (cadr form) expected-lib)
                   ) ;and
                   #t
                  ) ;
                  (else (loop (read port)))
            ) ;cond
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;guard
  ) ;and
) ;define

;; 辅助函数：检查文件是否包含 export 导出子句

(define (has-export-clause? file-path)
  (and (file-exists? file-path)
    (guard (e (else #f))
      (call-with-input-file file-path
        (lambda (port)
          (let loop
            ((form (read port)))
            (cond ((eof-object? form) #f)
                  ((and (pair? form) (eq? (car form) 'define-library))
                   (let check-body
                     ((body (cddr form)))
                     (cond ((null? body) #f)
                           ((and (pair? (car body)) (eq? (caar body) 'export)) #t)
                           (else (check-body (cdr body)))
                     ) ;cond
                   ) ;let
                  ) ;
                  (else (loop (read port)))
            ) ;cond
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;guard
  ) ;and
) ;define

;; 辅助函数：统计文件行数

(define (file-line-count path)
  (call-with-input-file path
    (lambda (port)
      (let loop
        ((count 0))
        (let ((line (read-line port)))
          (if (eof-object? line) count (loop (+ count 1)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; ============================================================
;; 验收点 1: 目标多级子库是否存在且可 import（缺失即失败项）
;; ============================================================
;; (liii pi types)
(check-true (sublibrary-ready? "tools/pi/liii/pi/types.scm" '(liii pi types)))
;; (liii pi gemini)
(check-true (sublibrary-ready? "tools/pi/liii/pi/gemini.scm" '(liii pi gemini)))
;; (liii pi anthropic)
(check-true (sublibrary-ready? "tools/pi/liii/pi/anthropic.scm" '(liii pi
                                                                   anthropic))
) ;check-true
;; (liii pi auth)
(check-true (sublibrary-ready? "tools/pi/liii/pi/auth.scm" '(liii pi auth)))
;; (liii pi models)
(check-true (sublibrary-ready? "tools/pi/liii/pi/models.scm" '(liii pi models)))
;; (liii pi retry)
(check-true (sublibrary-ready? "tools/pi/liii/pi/retry.scm" '(liii pi retry)))
;; (liii pi agent)
(check-true (sublibrary-ready? "tools/pi/liii/pi/agent.scm" '(liii pi agent)))
;; (liii pi tools)
(check-true (sublibrary-ready? "tools/pi/liii/pi/tools.scm" '(liii pi tools)))
;; (liii pi config)
(check-true (sublibrary-ready? "tools/pi/liii/pi/config.scm" '(liii pi config)))
;; (liii pi prompt)
(check-true (sublibrary-ready? "tools/pi/liii/pi/prompt.scm" '(liii pi prompt)))
;; (liii pi session)
(check-true (sublibrary-ready? "tools/pi/liii/pi/session.scm" '(liii pi session))
) ;check-true
;; (liii pi args)
(check-true (sublibrary-ready? "tools/pi/liii/pi/args.scm" '(liii pi args)))
;; (liii pi modes)
(check-true (sublibrary-ready? "tools/pi/liii/pi/modes.scm" '(liii pi modes)))

;; ============================================================
;; 验收点 2: 二级子库为导出层
;; ============================================================
(check-true (has-export-clause? "tools/pi/liii/pi/anthropic.scm"))
(check-true (has-export-clause? "tools/pi/liii/pi/config.scm"))
(check-true (has-export-clause? "tools/pi/liii/pi/prompt.scm"))

;; ============================================================
;; 验收点 3: 单文件不超 300 行硬约束
;; ============================================================
(check (run "bin/gf check" :timeout 60) => 0)
(check (<= (file-line-count "tools/pi/liii/pi.scm") 300) => #t)
(check (<= (file-line-count "tools/pi/liii/pi-cli.scm") 300) => #t)
(check (<= (file-line-count "tools/pi/liii/pi-agent.scm") 300) => #t)
(check (<= (file-line-count "tools/pi/liii/pi-provider.scm") 300) => #t)
(check (<= (file-line-count "tools/pi/liii/pi-args.scm") 300) => #t)

;; ============================================================
;; 验收点 4: CLI 侧无 goldpi* 残留
;; ============================================================
(check-false (file-exists? "tools/pi/liii/goldpi.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-args.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-config.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-provider.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-agent.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-tools.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-session.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-stream.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-stream-parse.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-anthropic.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-anthropic-stream.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-anthropic-parse.scm"))
(check-false (file-exists? "tools/pi/liii/goldpi-help.scm"))
(check-false (file-exists? "tools/pi/tests/liii/goldpi-cli-e2e-test.scm"))
(check-false (file-exists? "tools/pi/tests/liii/goldpi-args-test.scm"))

;; ============================================================
;; 验收点 5: gfproject.json 指向 (liii pi-cli)
;; ============================================================
(check
  (guard (e (else #f))
    (let* ((text
             (call-with-input-file "gfproject.json"
               (lambda (p)
                 (let loop
                   ((chars '()))
                   (let ((c (read-char p)))
                     (if (eof-object? c) (list->string (reverse chars)) (loop (cons c chars)))
                   ) ;let
                 ) ;let
               ) ;lambda
             ) ;call-with-input-file
           ) ;text
           (json (string->json text))
           (tools
             (and (pair? json)
               (cond ((assoc "tools" json) => cdr)
                     ((assoc 'tools json) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;tools
           (pi-tool
             (and (pair? tools)
               (cond ((assoc "pi" tools) => cdr)
                     ((assoc 'pi tools) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;pi-tool
           (mod-entry
             (and (pair? pi-tool)
               (cond ((assoc "module" pi-tool) => cdr)
                     ((assoc 'module pi-tool) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;mod-entry
          ) ;
      (and (string? mod-entry)
        (string=? mod-entry "pi-cli")
        (not (string-contains text "goldpi"))
      ) ;and
    ) ;let*
  ) ;guard
  =>
  #t
) ;check

;; ============================================================
;; 验收点 6: bin/gf test 全过
;; ============================================================
(check (run "bin/gf test tools/pi/tests/liii/pi-args-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-config-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-provider-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-agent-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-json-e2e-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-prompt-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-session-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-tools-basic-test.scm" :timeout 60)
  =>
  0
) ;check
(check (run "bin/gf test tools/pi/tests/liii/pi-tools-search-test.scm" :timeout 60)
  =>
  0
) ;check

;; ============================================================
;; 验收点 7: bin/gf pi --help 可用
;; ============================================================
(check (run "bin/gf pi --help" :timeout 30) => 0)

(check-report)

(import (scheme base)
  (scheme file)
  (scheme read)
  (liii check)
  (liii subprocess)
  (liii string)
) ;import

(check-set-mode! 'report-failed)

;; [016] 文档与实况一致性校验 验收脚本

(set! *load-path* (cons "tools/pi" *load-path*))

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
;; 验收点 1: PI_SPEC.md 架构演进与实况核对
;; ============================================================================

(check-true (file-exists? "PI_SPEC.md"))

(let ((spec-content (%read-file-content "PI_SPEC.md")))
  ;; 1.1 架构演进节中包含核心子库与 provider 描述
  (check-true (string-contains? spec-content "tools/pi/liii/"))
  (check-true (string-contains? spec-content "pi.scm"))
  (check-true (string-contains? spec-content "cli.scm"))
  (check-true (string-contains? spec-content "gemini.scm"))
  (check-true (string-contains? spec-content "anthropic.scm"))
  (check-true (string-contains? spec-content "kimi.scm"))
  (check-true (string-contains? spec-content "deepseek.scm"))
  (check-true (string-contains? spec-content "provider.scm"))
  ;; 1.2 包含已完成的 003-008, 015, 017-021 状态记录
  (check-true (string-contains? spec-content "✅ 任务 003"))
  (check-true (string-contains? spec-content "✅ 任务 008"))
  (check-true (string-contains? spec-content "✅ 任务 018"))
  (check-true (string-contains? spec-content "✅ 任务 019"))
  (check-true (string-contains? spec-content "✅ 任务 020"))
  (check-true (string-contains? spec-content "✅ 任务 021"))
  ;; 1.3 TODO 中已清除已完成的 provider
  (check-false (string-contains? spec-content "- [ ] **Kimi provider**"))
  ;; 1.4 Provider 支持范围包含已支持的 provider
  (check-true (string-contains? spec-content "kimi-coding（Kimi）"))
  (check-true (string-contains? spec-content "deepseek（DeepSeek）"))
  (check-true (string-contains? spec-content "glm（智谱 GLM）"))
  ;; 1.5 对应的二级库与实现文件在磁盘中确实存在
  (check-true (file-exists? "tools/pi/liii/pi.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/cli.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/gemini.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/anthropic.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/kimi.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/deepseek.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/provider.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/types.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/message.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/event.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/tools.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/config.scm"))
  (check-true (file-exists? "tools/pi/liii/pi/models.scm"))
) ;let

;; ============================================================================
;; 验收点 2: gfproject.json 与 CLI 入口实况核对
;; ============================================================================

(check-true (file-exists? "gfproject.json"))
(let ((gfproject-content (%read-file-content "gfproject.json")))
  (check-true (string-contains? gfproject-content "\"pi\":"))
  (check-true (string-contains? gfproject-content "\"module\": \"pi/cli\""))
) ;let

(let* ((tmp-help "/tmp/pi-016-help.txt")
       (code (run (string-append "sh -c 'bin/gf pi --help > " tmp-help "'") :timeout 30)
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-help)))
    (check-true (string-contains? out "gf pi - AI coding assistant script mode"))
    (check-true (string-contains? out "--list-models"))
  ) ;let
) ;let*

;; ============================================================================
;; 验收点 3: PI_SPEC 示例逐条复核 (--list-models 等)
;; ============================================================================

;; 3.1 默认 provider 列出模型
(let* ((tmp-out "/tmp/pi-016-list-all.txt")
       (code (run (string-append "sh -c 'bin/gf pi --list-models > " tmp-out "'")
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "provider"))
    (check-true (string-contains? out "model"))
    (check-true (string-contains? out "google"))
  ) ;let
) ;let*

;; 3.2 过滤 kimi-coding
(let* ((tmp-out "/tmp/pi-016-list-kimi.txt")
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
    (check-true (string-contains? out "kimi-for-coding"))
  ) ;let
) ;let*

;; 3.3 搜索过滤词 flash
(let* ((tmp-out "/tmp/pi-016-list-flash.txt")
       (code (run (string-append "sh -c 'bin/gf pi --list-models flash > " tmp-out "'")
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "flash"))
  ) ;let
) ;let*

;; ============================================================================
;; 验收点 4: bin/gf doc 索引重建与抽查
;; ============================================================================

;; 4.1 全量重建
(let* ((tmp-out "/tmp/pi-016-doc-build.txt")
       (code (run (string-append "sh -c 'bin/gf doc --build-json > " tmp-out "'")
               :timeout 60
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "Built function index:"))
  ) ;let
) ;let*

;; 4.2 抽查核心库
(let* ((tmp-out "/tmp/pi-016-doc-substring.txt")
       (code (run (string-append "sh -c 'bin/gf doc substring > " tmp-out "'") :timeout 30)
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "substring"))
    (check-true (string-contains? out "scheme base"))
  ) ;let
) ;let*

;; 4.3 抽查 tools/pi 下函数
(let* ((tmp-out "/tmp/pi-016-doc-pi.txt")
       (code (run (string-append "sh -c 'bin/gf -I tools/pi doc pi-event-session > " tmp-out "'")
               :timeout 30
             ) ;run
       ) ;code
      ) ;
  (check code => 0)
  (let ((out (%read-file-content tmp-out)))
    (check-true (string-contains? out "pi-event-session"))
  ) ;let
) ;let*

;; ============================================================================
;; 验收点 5: devel 任务文档状态与实况核对
;; ============================================================================

(let ((tasks '("001" "002" "003" "004" "005" "006" "007" "008" "015" "017" "018"
               "019" "020" "021")
      ) ;tasks
     ) ;
  (for-each
    (lambda (id)
      (let* ((md-path (string-append "tools/pi/devel/" id ".md"))
             (scm-path (string-append "tools/pi/devel/" id ".scm"))
            ) ;
        (check-true (file-exists? md-path))
        (check-true (file-exists? scm-path))
        (let ((content (%read-file-content md-path)))
          (check-true (string-contains? content "## 状态\n\n已完成"))
        ) ;let
      ) ;let*
    ) ;lambda
    tasks
  ) ;for-each
) ;let

(check-true (file-exists? "tools/pi/devel/016.md"))

;; ============================================================================
;; 验收点 6: 门禁与测试
;; ============================================================================

(check (run "timeout 600 xmake b goldfish" :timeout 60) => 0)
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

(check-report)

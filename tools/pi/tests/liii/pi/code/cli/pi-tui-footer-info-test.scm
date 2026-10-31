(import (liii check)
  (liii os)
  (liii path)
  (liii pi code cli args)
  (liii pi code cli tui)
) ;import

(check-set-mode! 'report-failed)

;; pi-tui-footer-info
;; TUI 页脚与 make-pi-app 共用的启动配置提取。
;;
;; 语法
;; ----
;; (pi-tui-footer-info parsed) -> list
;;
;; 参数
;; ----
;; parsed : parse-pi-args 的解析结果。
;;
;; 返回值
;; ----
;; (provider model-id thinking-level supports-thinking default-thinking-level
;;  config-dir models default-model hide-thinking model-thinking-levels)
;;
;; 注意
;; ----
;; 该函数用 catch 兜住配置解析异常并回退到 CLI 原始参数，因此**兜底值一旦
;; 与真实配置不符，就无法从返回值区分「解析成功」和「解析失败」**。
;; 本测试用真实 fixture 目录断言 provider/model/thinking-level 均来自
;; settings.json，防止库漏导出符号（unbound variable）导致静默回落到
;; google/gemini-3.8-flash。
;;
;; 示例
;; ----
;; (car (pi-tui-footer-info (parse-pi-args '()))) => "google"

(define FIXTURE-DIR
  (if (file-exists? "tests/fixtures/model-thinking")
    "tests/fixtures/model-thinking"
    "tools/pi/tests/fixtures/model-thinking"
  ) ;if
) ;define

;; 用 putenv 让 pi-tui-footer-info 内部的 get-environment-variables 命中 fixture；
;; 结束时还原，避免污染同进程内后续测试
(define (with-config-dir thunk)
  (let ((saved (getenv "GF_CONFIG_DIR"))
        (saved-pi (getenv "PI_CODING_AGENT_DIR")))
    (putenv "GF_CONFIG_DIR" (path->string (path-absolute (path FIXTURE-DIR))))
    (unsetenv "PI_CODING_AGENT_DIR")
    (let ((result (thunk)))
      (if saved
        (putenv "GF_CONFIG_DIR" saved)
        (unsetenv "GF_CONFIG_DIR")
      ) ;if
      (when saved-pi (putenv "PI_CODING_AGENT_DIR" saved-pi))
      result
    ) ;let
  ) ;let
) ;define

;; 1. provider / model 来自 settings.json（不是兜底的 google/gemini-3.8-flash）
(let ((info (with-config-dir (lambda () (pi-tui-footer-info (parse-pi-args '()))))))
  (check (list-ref info 0) => "google")
  (check (list-ref info 1) => "gemini-3.8-flash")
  ;; settings.defaultThinkingLevel = "low"，被 per-model 覆盖为 "high"
  (check (list-ref info 2) => "high")
  (check (list-ref info 3) => #t)
  ;; 默认级别为 settings 原始值
  (check (list-ref info 4) => "low")
)

;; 2. per-model 覆盖表被原样传给 make-pi-app（/model 切换时使用）
(let ((info (with-config-dir (lambda () (pi-tui-footer-info (parse-pi-args '()))))))
  (check (list-ref info 9)
    =>
    '(("google/gemini-3.8-flash" . "high") ("google/plain-model" . "max"))
  ) ;check
) ;let

;; 3. 显式 --thinking 压过 per-model 覆盖与全局默认
(let ((info (with-config-dir
              (lambda () (pi-tui-footer-info (parse-pi-args '("--thinking" "minimal"))))
            ) ;with-config-dir
      ) ;
     ) ;
  (check (list-ref info 2) => "minimal")
) ;let

;; 4. 解析失败时的兜底口径：思考级别回退到默认级别 medium，不再是 "off"
;;    （对齐上游 DEFAULT_THINKING_LEVEL；用不存在的目录强制走兜底分支）
(let ((saved (getenv "GF_CONFIG_DIR"))
      (saved-pi (getenv "PI_CODING_AGENT_DIR")))
  (putenv "GF_CONFIG_DIR" "/nonexistent-gf-config-dir-for-test")
  (unsetenv "PI_CODING_AGENT_DIR")
  (let ((info (pi-tui-footer-info (parse-pi-args '()))))
    ;; 无法解析配置时回退到硬编码兜底，thinking level 为 medium 而非 off
    (check (list-ref info 2) => "medium")
    (check (list-ref info 3) => #t)
  ) ;let
  (if saved (putenv "GF_CONFIG_DIR" saved) (unsetenv "GF_CONFIG_DIR"))
  (when saved-pi (putenv "PI_CODING_AGENT_DIR" saved-pi))
) ;let

(check-report)

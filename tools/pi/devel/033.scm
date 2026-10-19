(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  ;; 显式导入 (scheme process-context)：get-environment-variable 未被 (liii string)
  ;; 等导出，若不显式导入会解析到 define-library 泄漏的破损绑定并段错误（任务 038）
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
  (liii pi args)
  (liii pi config)
) ;import

(check-set-mode! 'report-failed)

;; [033] 验收脚本：multikey.json 读取/过滤 / cooldown+LRU / 脱敏 /
;; TDD 单测与流式+非流式轮换 e2e / 门禁。外部命令一律 run :timeout 包裹（≤300s）。

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%fixture-dir name)
  (if (file-exists? (string-append "tests/fixtures/" name))
    (string-append "tests/fixtures/" name)
    (string-append "tools/pi/tests/fixtures/" name)
  ) ;if
) ;define

(define mk-dir (%fixture-dir "multikey"))

(define disabled-dir (%fixture-dir "multikey-disabled"))

(define e2e-dir (%fixture-dir "multikey-e2e"))

(define nopool-dir (%fixture-dir "multikey-nopool"))

(define direct-dir (%fixture-dir "direct"))

(display "=== 验收点 1: multikey.json 读取与 enabled 过滤 ===\n")

;; 1.1 命中 pool：gamma(enabled=false) 被过滤，冷却时长取 pool 配置
(let ((pool (pi-load-multikey mk-dir "kimi-coding")))
  (check-true (pi-key-pool? pool))
  (check (pi-key-pool-id pool) => "kimi-coding")
  (check (map pi-pool-key-label (pi-key-pool-keys pool)) => '("alpha" "beta"))
  (check (length (pi-key-pool-keys pool)) => 2)
  (check (pi-key-pool-cooldown-ms pool) => 30000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 300000)
) ;let

;; 1.2 pool 未配置冷却时长：429→20000，403/invalid→600000
(let ((pool (pi-load-multikey mk-dir "google")))
  (check-true (pi-key-pool? pool))
  (check (map pi-pool-key-label (pi-key-pool-keys pool)) => '("g1" "g2"))
  (check (pi-key-pool-cooldown-ms pool) => 20000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 600000)
) ;let

;; 1.3 无对应 pool / 文件不存在 / 全部禁用：均返回 #f（调用方回退 auth.json）
(check (pi-load-multikey mk-dir "anthropic") => #f)
(check (pi-load-multikey disabled-dir "google") => #f)
(check (pi-load-multikey direct-dir "google") => #f)
(check (pi-load-multikey nopool-dir "google") => #f)

;; 1.4 无 pool 时回退 auth.json，配置层 api-key 与现状一致
(let ((auth (pi-load-auth nopool-dir)))
  (check (pi-resolve-provider-key "google" auth) => "plain-google-key-xxxx")
) ;let
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-3.8-flash")))
       (cfg
         (resolve-pi-config args `(("PI_CODING_AGENT_DIR" . ,nopool-dir)))
       ) ;cfg
      ) ;
  (check (cdr (assoc 'multikey-pool cfg)) => #f)
  (check (cdr (assoc 'api-key cfg)) => "plain-google-key-xxxx")
) ;let*

;; 1.5 有 pool 时配置层优先使用 pool key（覆盖 auth.json 的 key）
(let* ((args (parse-pi-args '("--provider" "google" "--model" "gemini-3.8-flash")))
       (cfg
         (resolve-pi-config args `(("PI_CODING_AGENT_DIR" . ,e2e-dir)))
       ) ;cfg
       (pool (cdr (assoc 'multikey-pool cfg)))
      ) ;
  (check-true (pi-key-pool? pool))
  (check (pi-key-pool-id pool) => "google")
  (check (cdr (assoc 'api-key cfg)) => "mk-gem-key-1-aaaaaa")
) ;let*

(display "=== 验收点 2: cooldown + LRU 状态机（429→20s / 403→600s） ===\n"
) ;display

;; 2.1 默认冷却：429→20000ms，403→600000ms
(let* ((pool (pi-load-multikey mk-dir "google"))
       (k1 (car (pi-key-pool-keys pool)))
       (k2 (cadr (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k1 429 now pool)
  (pi-multikey-mark-cooldown! k2 403 now pool)
  (check-true (pi-pool-key-cooling? k1 (+ now 19999)))
  (check-true (not (pi-pool-key-cooling? k1 (+ now 20000))))
  (check-true (pi-pool-key-cooling? k2 (+ now 599999)))
  (check-true (not (pi-pool-key-cooling? k2 (+ now 600000))))
) ;let*

;; 2.2 pool 配置覆盖：429→30000ms，403→300000ms
(let* ((pool (pi-load-multikey mk-dir "kimi-coding"))
       (k1 (car (pi-key-pool-keys pool)))
       (k2 (cadr (pi-key-pool-keys pool)))
       (now 1000000)
      ) ;
  (pi-multikey-mark-cooldown! k1 429 now pool)
  (pi-multikey-mark-cooldown! k2 403 now pool)
  (check-true (pi-pool-key-cooling? k1 (+ now 29999)))
  (check-true (not (pi-pool-key-cooling? k1 (+ now 30000))))
  (check-true (pi-pool-key-cooling? k2 (+ now 299999)))
  (check-true (not (pi-pool-key-cooling? k2 (+ now 300000))))
) ;let*

;; 2.3 LRU：连续选取在两个 key 之间轮换（最久未用优先）
(let* ((pool (pi-load-multikey mk-dir "kimi-coding"))
       (s1 (pi-multikey-select-key pool 1000000))
       (s2 (pi-multikey-select-key pool 1000001))
       (s3 (pi-multikey-select-key pool 1000002))
      ) ;
  (check (pi-pool-key-label s1) => "alpha")
  (check (pi-pool-key-label s2) => "beta")
  (check (pi-pool-key-label s3) => "alpha")
) ;let*

;; 2.4 冷却中的 key 被过滤；全部冷却（耗尽）时返回 #f
(let* ((pool (pi-load-multikey mk-dir "kimi-coding"))
       (alpha (car (pi-key-pool-keys pool)))
       (now 2000000)
      ) ;
  (pi-multikey-mark-cooldown! alpha 429 now pool)
  (check (pi-pool-key-label (pi-multikey-select-key pool now)) => "beta")
) ;let*
(let* ((pool (pi-load-multikey mk-dir "google"))
       (keys (pi-key-pool-keys pool))
       (now 3000000)
      ) ;
  (pi-multikey-mark-cooldown! (car keys) 429 now pool)
  (pi-multikey-mark-cooldown! (cadr keys) 403 now pool)
  (check (pi-multikey-select-key pool now) => #f)
) ;let*

;; 2.5 轮换：429 冷却当前 key 换下一 key；全部冷却返回 'exhausted；
;;     每请求重新选取，跨请求不复用冷却中的 key
(let* ((pool (pi-load-multikey mk-dir "kimi-coding"))
       (cfg (list (cons 'multikey-pool pool) (cons 'api-key "sk-pool-alpha-0001")))
       (alpha (car (pi-key-pool-keys pool)))
      ) ;
  (check (pi-multikey-rotate! cfg "sk-pool-alpha-0001" 429)
    =>
    "sk-pool-beta-00002"
  ) ;check
  (check-true (pi-pool-key-cooling? alpha (pi-current-time-millis)))
  (check (pi-multikey-request-key cfg) => "sk-pool-beta-00002")
  (check (pi-multikey-rotate! cfg "sk-pool-beta-00002" 403) => 'exhausted)
) ;let*

;; 2.6 无 pool：request-key 返回 auth.json key，rotate 不改变状态
(let ((cfg (list (cons 'multikey-pool #f) (cons 'api-key "auth-json-key"))))
  (check (pi-multikey-request-key cfg) => "auth-json-key")
  (check (pi-multikey-rotate! cfg "auth-json-key" 429) => #f)
) ;let

(display "=== 验收点 3: 日志脱敏（完整 key 不出现在输出/错误摘要） ===\n"
) ;display

(check (pi-mask-key "sk-kimi-abcdefghij8ByO") => "sk-kim...8ByO")
(check (pi-mask-key "mk-gem-key-1-aaaaaa") => "mk-gem...aaaa")
(check (pi-mask-key "12345678901") => "123456...8901")
(check (pi-mask-key "1234567890") => "***")
(check (pi-mask-key "") => "***")
(check (pi-mask-key #f) => "***")

;; e2e 用例对 stdout/stderr 做了完整 key 缺席断言（403/429/耗尽三类）
(let ((e2e (path-read-text "tools/pi/tests/liii/pi-multikey-e2e-test.scm")))
  (check-true (string-contains? e2e
                "(not (string-contains? (caddr r) \"mk-gem-key-1-aaaaaa\"))"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? e2e
                "(not (string-contains? (caddr r) \"mk-gem-key-2-bbbbbb\"))"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? e2e
                "(not (string-contains? (caddr r) \"mk-kimi-key-1-aaaaaa\"))"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? e2e
                "(not (string-contains? (cadr r) \"mk-gem-key-1-aaaaaa\"))"
              ) ;string-contains?
  ) ;check-true
) ;let

(display "=== 验收点 4: TDD 单测 + 流式/非流式轮换 e2e ===\n")
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-load-multikey-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-mask-key-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-multikey-select-key-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-multikey-mark-cooldown-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-multikey-request-key-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "timeout 120 bin/gf test tools/pi/tests/liii/pi/multikey/pi-multikey-rotate-test.scm"
         150
       ) ;%run-shell
  =>
  0
) ;check
;; 403/429 × 非流式/流式（Gemini + Kimi）、pool 耗尽、无 pool 回退 auth.json
(check (%run-shell "timeout 240 bin/gf test tools/pi/tests/liii/pi-multikey-e2e-test.scm"
         280
       ) ;%run-shell
  =>
  0
) ;check
;; 全量回归（既有 144 个测试文件不因轮换接入而失败）
(check (%run-shell "timeout 240 bin/gf test tools/pi/tests/liii" 280) => 0)

;; e2e 场景与实现接入点存在性
(let ((e2e (path-read-text "tools/pi/tests/liii/pi-multikey-e2e-test.scm"))
      (stub (path-read-text "tools/pi/tests/stub_gemini_server.py"))
      (gem-req (path-read-text "tools/pi/liii/pi/gemini/request.scm"))
      (gem-str (path-read-text "tools/pi/liii/pi/gemini/stream.scm"))
      (kimi-req (path-read-text "tools/pi/liii/pi/kimi/request.scm"))
      (kimi-str (path-read-text "tools/pi/liii/pi/kimi/stream.scm"))
     ) ;
  (check-true (string-contains? e2e "multikey-403"))
  (check-true (string-contains? e2e "multikey-429"))
  (check-true (string-contains? e2e "multikey-stream-403"))
  (check-true (string-contains? e2e "multikey-stream-429"))
  (check-true (string-contains? e2e "multikey-all-403"))
  (check-true (string-contains? e2e "multikey-nopool"))
  (check-true (string-contains? e2e "mk-kimi-stream-403"))
  (check-true (string-contains? e2e "mk-kimi-stream-429"))
  (check-true (string-contains? stub "/mk-kimi-all-403"))
  (check-true (string-contains? gem-req "pi-multikey-rotate!"))
  (check-true (string-contains? gem-req "pi-multikey-request-key"))
  (check-true (string-contains? gem-str "pi-multikey-rotate!"))
  (check-true (string-contains? gem-str "(not emitted?)"))
  (check-true (string-contains? kimi-req "pi-multikey-rotate!"))
  (check-true (string-contains? kimi-str "pi-multikey-rotate!"))
) ;let

(display "=== 验收点 5: 门禁检查 ===\n")
(check (%run-shell "timeout 290 xmake b goldfish" 300) => 0)
(check (%run-shell "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-shell "timeout 120 bin/gf check" 120) => 0)
(check (%run-shell "timeout 60 bin/gf doc --build-json" 60) => 0)

(check-report)

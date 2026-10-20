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
) ;import

(check-set-mode! 'report-failed)

;; [030] 验收脚本：MiniMax provider 适配
;; 逐条对应 tools/pi/devel/030.md 的验收点：
;;   1) 模型目录元数据（函数级）：minimax / minimax-cn 四模型、大陆站端点
;;   2) CLI 模型目录：--list-models（两个 provider，退出码 0）
;;   3) 凭据与端点：缺失凭据明确报错；元数据 baseUrl 为 api.minimaxi.com
;;   4) 协议映射相关 TDD 单测 + 全量回归
;;   5) 真实冒烟：MiniMax-M3.1-Flash 纯文本（minimax / minimax-cn）
;;   6) 真实冒烟：--tools read,bash,write 工具调用闭环
;;   7) 门禁：构建 / fmt --check / check / doc 索引
;; 外部命令一律 run :timeout 包裹，单条命令超时不超过 300s。

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

;; 运行命令并把 stdout/stderr 落到临时文件，返回 (退出码 . 输出文本)

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-030-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      ;; 注：path-read-text 读取目录会触发 goldfish 内核段错误，须先用 path-file? 守卫
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

;; 只关心退出码的便捷封装

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(define (%assoc-list-key key alist)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) #f)
  ) ;let
) ;define

(define (%builtin-model-ids provider)
  (let ((entry (assoc provider PI_PROVIDER_BUILTIN_MODELS)))
    (if (not entry) '() (map (lambda (m) (%assoc-list-key "id" m)) (cdr entry)))
  ) ;let
) ;define

(display "=== 验收点 1: 模型目录元数据（minimax / minimax-cn 四模型） ===\n"
) ;display

;; 1.1 provider 已注册
(check-true (pair? (member "minimax" PI_SUPPORTED_PROVIDERS)))
(check-true (pair? (member "minimax-cn" PI_SUPPORTED_PROVIDERS)))

;; 1.2 内置目录包含用户裁定的四个模型
(check (%builtin-model-ids "minimax")
  =>
  '("MiniMax-M3.1-Flash" "MiniMax-M3" "MiniMax-M2.7" "MiniMax-M2.7-highspeed")
) ;check
(check (%builtin-model-ids "minimax-cn")
  =>
  '("MiniMax-M3.1-Flash" "MiniMax-M3" "MiniMax-M2.7" "MiniMax-M2.7-highspeed")
) ;check

;; 1.3 默认模型为 M3.1-Flash（用户裁定主力）
(check (pi-provider-default-model "minimax") => "MiniMax-M3.1-Flash")
(check (pi-provider-default-model "minimax-cn") => "MiniMax-M3.1-Flash")

;; 1.4 元数据命中：M3.1-Flash 1M/524288，M2.7 系列 204.8K/131072
(let ((meta (pi-resolve-model-metadata "minimax" "MiniMax-M3.1-Flash" '() '())))
  (check (%assoc-list-key 'matched? meta) => #t)
  (check (%assoc-list-key 'base-url meta) => "https://api.minimaxi.com/anthropic")
  (check (%assoc-list-key 'context-window meta) => 1000000)
  (check (%assoc-list-key 'max-tokens meta) => 524288)
) ;let
(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3" '() '())))
  (check (%assoc-list-key 'matched? meta) => #t)
  (check (%assoc-list-key 'base-url meta) => "https://api.minimaxi.com/anthropic")
  (check (%assoc-list-key 'context-window meta) => 1000000)
  (check (%assoc-list-key 'max-tokens meta) => 512000)
) ;let
(let ((meta (pi-resolve-model-metadata "minimax" "MiniMax-M2.7" '() '())))
  (check (%assoc-list-key 'matched? meta) => #t)
  (check (%assoc-list-key 'context-window meta) => 204800)
  (check (%assoc-list-key 'max-tokens meta) => 131072)
) ;let
(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M2.7-highspeed" '() '()))
     ) ;
  (check (%assoc-list-key 'matched? meta) => #t)
  (check (%assoc-list-key 'context-window meta) => 204800)
  (check (%assoc-list-key 'max-tokens meta) => 131072)
) ;let

;; 1.5 models-store 中国际站 baseUrl 归一为大陆站
(let* ((store '(("minimax"
                 ("models"
                  . #((("id" . "MiniMax-M3")
                       ("baseUrl" . "https://api.minimax.io/anthropic")
                       ("contextWindow" . 1000000)
                       ("maxTokens" . 512000))
                    ) ;#
                 )))
       ) ;store
       (meta (pi-resolve-model-metadata "minimax" "MiniMax-M3" store '()))
      ) ;
  (check (%assoc-list-key 'base-url meta) => "https://api.minimaxi.com/anthropic")
) ;let*

(display "=== 验收点 2: CLI 模型目录（--list-models，两个 provider） ===\n"
) ;display
(let ((rc (%run-capture "list-models"
            "timeout 60 bin/gf pi --provider minimax --list-models"
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "MiniMax-M3.1-Flash"))
  (check-true (string-contains? (cdr rc) "MiniMax-M3"))
  (check-true (string-contains? (cdr rc) "MiniMax-M2.7"))
  (check-true (string-contains? (cdr rc) "MiniMax-M2.7-highspeed"))
  (check-false (string-contains? (cdr rc) "Unknown provider"))
) ;let
(let ((rc (%run-capture "list-models-cn"
            "timeout 60 bin/gf pi --provider minimax-cn --list-models"
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "MiniMax-M3.1-Flash"))
  (check-true (string-contains? (cdr rc) "MiniMax-M3"))
  (check-true (string-contains? (cdr rc) "MiniMax-M2.7"))
  (check-true (string-contains? (cdr rc) "MiniMax-M2.7-highspeed"))
  (check-false (string-contains? (cdr rc) "Unknown provider"))
) ;let

(display "=== 验收点 3: 凭据缺失明确报错（绝不静默成功） ===\n"
) ;display
(let ((rc (%run-capture "no-cred"
            "env PI_CODING_AGENT_DIR=/nonexistent HOME=/nonexistent timeout 60 bin/gf pi --provider minimax -p hi"
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 1)
  (check-true (string-contains? (cdr rc) "No credentials found for provider"))
  (check-true (string-contains? (cdr rc) "minimax"))
) ;let

;; 3.2 凭据存在时上游接受大陆站端点与 sk-cp- 凭据（由验收点 5 的真实冒烟覆盖）

(display "=== 验收点 4: 相关 TDD 单测 + 全量回归 ===\n")
(check (%run-ok? "test-minimax"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/minimax/"
         150
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-builtin"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/models/builtin/"
         150
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-route"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/models/route/"
         150
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-meta"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/pi-resolve-model-metadata-test.scm"
         150
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-key"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/pi-resolve-provider-key-test.scm"
         150
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-cost"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/cost/pi-model-cost-test.scm"
         150
       ) ;%run-ok?
  =>
  0
) ;check

;; 4.7 全量回归：既有 150 个 pi 测试文件不因新增 provider 失败
(let ((rc (%run-capture "test-all" "timeout 240 bin/gf test tools/pi/tests/liii/pi" 280)
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "Passed: 150"))
) ;let

(display "=== 验收点 5: 真实冒烟（MiniMax-M3.1-Flash 纯文本） ===\n"
) ;display
(let ((rc (%run-capture "smoke-text"
            "timeout 120 bin/gf pi --provider minimax --model MiniMax-M3.1-Flash -p \"只回复：minimax-ok\""
            150
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "minimax-ok"))
  (check-false (string-contains? (cdr rc) "not found in models store"))
) ;let

;; 5.2 minimax-cn 别名 + 大陆站端点同样可用
(let ((rc (%run-capture "smoke-text-cn"
            "timeout 120 bin/gf pi --provider minimax-cn --model MiniMax-M3.1-Flash -p \"只回复：minimax-cn-ok\""
            150
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "minimax-cn-ok"))
) ;let

(display "=== 验收点 6: 真实冒烟（--tools read,bash,write 工具调用） ===\n"
) ;display
(when (file-exists? "tmp-030-smoke.txt")
  (delete-file "tmp-030-smoke.txt")
) ;when
(let ((rc (%run-capture "smoke-tools"
            "timeout 280 bin/gf pi --provider minimax --model MiniMax-M3.1-Flash --tools read,bash,write -p \"用 read 工具读取 tools/pi/devel/030.md 前 20 行；用 bash 工具执行 git status --short；用 write 工具把字符串 030-tool-ok 写入 tmp-030-smoke.txt；最后只回复该文件内容\""
            295
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "030-tool-ok"))
  (check-false (string-contains? (cdr rc) "not found in models store"))
) ;let
;; write 工具确实落盘
(check-true (file-exists? "tmp-030-smoke.txt"))
(when (file-exists? "tmp-030-smoke.txt")
  (check-true (string-contains? (path-read-text "tmp-030-smoke.txt") "030-tool-ok")
  ) ;check-true
  (delete-file "tmp-030-smoke.txt")
) ;when

(display "=== 验收点 7: 门禁检查 ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt" "timeout 180 bin/gf fmt --check" 200) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 150) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 150)
  =>
  0
) ;check
;; 函数索引包含 minimax-*
(let ((rc (%run-capture "doc-query"
            "timeout 60 bin/gf -I tools/pi doc minimax-parse-response"
            90
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "minimax-parse-response"))
) ;let

(check-report)

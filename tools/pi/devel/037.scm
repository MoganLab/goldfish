(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [037] 验收脚本：--list-models 与 pi 完整对齐

(define (%unique lst)
  (let loop
    ((rest lst) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((member (car rest) acc) (loop (cdr rest) acc))
          (else (loop (cdr rest) (cons (car rest) acc)))
    ) ;cond
  ) ;let
) ;define

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp (string-append "tmp-037-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp) (path-file? (path tmp))) (path-read-text tmp) "")
            ) ;out
           ) ;
        (when (file-exists? tmp)
          (delete-file tmp)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(define (%parse-model-pairs text)
  (let loop
    ((rest (string-split text "\n")) (acc '()))
    (if (null? rest)
      (reverse acc)
      (let ((toks (pi-split-search-tokens (car rest))))
        (if (or (null? toks) (string=? (car toks) "provider") (string=? (car toks) "No"))
          (loop (cdr rest) acc)
          (loop (cdr rest) (cons (cons (car toks) (cadr toks)) acc))
        ) ;if
      ) ;let
    ) ;if
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc / 行数约束） ===\n"
) ;display
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 120)
  =>
  0
) ;check

(let ((files '("tools/pi/liii/pi/models/builtin.scm"
               "tools/pi/liii/pi/models/builtin/anthropic.scm"
               "tools/pi/liii/pi/models/builtin/deepseek.scm"
               "tools/pi/liii/pi/models/builtin/google.scm"
               "tools/pi/liii/pi/models/builtin/kimi-coding.scm"
               "tools/pi/liii/pi/models/builtin/minimax.scm"
               "tools/pi/liii/pi/models/builtin/minimax-cn.scm"
               "tools/pi/liii/pi/models/builtin/zai-coding-cn.scm"
               "tools/pi/liii/pi/models/catalog.scm"
               "tools/pi/liii/pi/models/list.scm"
               "tools/pi/liii/pi/models/meta.scm"
               "tools/pi/liii/pi/models/route.scm"
               "tools/pi/liii/pi/models/search.scm" "tools/pi/liii/pi/cli.scm"
               "tools/pi/liii/pi/config.scm" "tools/pi/devel/037.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (check-true
        (<= (length (string-split (path-read-text f) "\n")) 300)
      ) ;check-true
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 2: 单元测试全量回归（172 测试全绿） ===\n"
) ;display
(let ((rc (%run-capture "test-all" "timeout 240 bin/gf test tools/pi/tests/liii/" 260)
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "Passed: 172"))
) ;let

(display "=== 验收点 3: 模型集合与 pi 本尊 100% 对齐（8 provider / 70 模型） ===\n"
) ;display
(let* ((gf-res (%run-capture "gf-list" "timeout 60 bin/gf pi --list-models" 60))
       (pi-res (%run-capture "pi-list" "timeout 60 pi --list-models" 60))
       (gf-pairs (%parse-model-pairs (cdr gf-res)))
       (pi-pairs (%parse-model-pairs (cdr pi-res)))
      ) ;
  (check (car gf-res) => 0)
  (check (car pi-res) => 0)
  (check (length gf-pairs) => 70)
  (check (length pi-pairs) => 70)
  (check gf-pairs => pi-pairs)
  (let ((provs (%unique (map car gf-pairs))))
    (check (length provs) => 8)
    (for-each
      (lambda (p) (check-true (pair? (member p provs))))
      '("anthropic" "deepseek" "glm" "google" "kimi-coding" "minimax"
        "minimax-cn" "zai-coding-cn")
    ) ;for-each
  ) ;let
) ;let*

(display "=== 验收点 4: 合并目录优先级（models.json > models-store.json > 内置） ===\n"
) ;display
(let* ((agent-dir (pi-get-agent-dir))
       (catalog (pi-build-models-catalog agent-dir))
       (provs (pi-catalog-provider-names catalog))
      ) ;
  (check (length provs) => 8)
  (let* ((base
           '(("test-p" (("id" . "m1") ("name" . "base"))))
         ) ;base
         (over
           '(("test-p" (("id" . "m1") ("name" . "over"))))
         ) ;over
         (m (pi-catalog-merge-provider-models (cdr (assoc "test-p" base))
              (cdr (assoc "test-p" over))
            ) ;pi-catalog-merge-provider-models
         ) ;m
        ) ;
    (check (cdr (assoc "name" (car m))) => "over")
  ) ;let*
) ;let*

(display "=== 验收点 5: 按 provider 过滤与未知 provider 报错退出 1 ===\n"
) ;display
(let* ((rc (%run-capture "zai"
             "timeout 60 bin/gf pi --provider zai-coding-cn --list-models"
             60
           ) ;%run-capture
       ) ;rc
       (pairs (%parse-model-pairs (cdr rc)))
      ) ;
  (check (car rc) => 0)
  (check (length pairs) => 10)
  (for-each (lambda (p) (check (car p) => "zai-coding-cn")) pairs)
) ;let*
(let* ((rc (%run-capture "goog" "timeout 60 bin/gf pi --provider google --list-models" 60)
       ) ;rc
       (pairs (%parse-model-pairs (cdr rc)))
      ) ;
  (check (car rc) => 0)
  (check (length pairs) => 23)
  (for-each (lambda (p) (check (car p) => "google")) pairs)
) ;let*
(let ((rc (%run-capture "err"
            "timeout 60 bin/gf pi --provider unknown-prov-xyz-037 --list-models"
            60
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 1)
  (check-true (string-contains? (cdr rc) "Unknown provider \"unknown-prov-xyz-037\"")
  ) ;check-true
) ;let

(display "=== 验收点 6: 格式对齐（pi-format-token-count 与表头） ===\n"
) ;display
(check (pi-format-token-count 1000000) => "1M")
(check (pi-format-token-count 1048576) => "1.0M")
(check (pi-format-token-count 1250000) => "1.3M")
(check (pi-format-token-count 131072) => "131.1K")
(check (pi-format-token-count 64000) => "64K")
(check (pi-format-token-count 8192) => "8.2K")
(check (pi-format-token-count 4096) => "4.1K")
(check (pi-format-token-count 524288) => "524.3K")
(check (pi-format-token-count 512000) => "512K")
(check (pi-format-token-count 500) => "500")
(check (pi-format-token-count 0) => "0")
(let ((rc (%run-capture "hdr" "timeout 60 bin/gf pi --list-models" 60)))
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc)
                "provider       model                                    context  max-out  thinking  images"
              ) ;string-contains?
  ) ;check-true
) ;let

(display "=== 验收点 7: 搜索语义对齐（子序列模糊匹配） ===\n")
(check-true (pi-fuzzy-match-model? "g25f" "google gemini-2.5-flash"))
(check-true (pi-fuzzy-match-model? "g25f" "google gemini-2.5-flash-lite"))
(check-false (pi-fuzzy-match-model? "g25f" "anthropic claude-sonnet-4-5"))
(let* ((rc (%run-capture "g25f" "timeout 60 bin/gf pi --list-models g25f" 60))
       (pairs (%parse-model-pairs (cdr rc)))
      ) ;
  (check (car rc) => 0)
  (check (length pairs) => 2)
  (check (cdr (car pairs)) => "gemini-2.5-flash")
  (check (cdr (cadr pairs)) => "gemini-2.5-flash-lite")
) ;let*
(let* ((rc (%run-capture "flash" "timeout 60 bin/gf pi --list-models flash" 60))
       (pairs (%parse-model-pairs (cdr rc)))
      ) ;
  (check (car rc) => 0)
  (check (length pairs) => 21)
  (let ((search-provs (%unique (map car pairs))))
    (for-each
      (lambda (p) (check-true (pair? (member p search-provs))))
      '("deepseek" "glm" "google" "minimax" "zai-coding-cn")
    ) ;for-each
  ) ;let
) ;let*
(let ((rc (%run-capture "none"
            "timeout 60 bin/gf pi --list-models nonexistentpattern999"
            60
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "No models matching \"nonexistentpattern999\"")
  ) ;check-true
) ;let

(display "=== 验收点 8: 默认模型路由与回退兜底 ===\n")
(check (pi-provider-default-model "anthropic") => "claude-opus-4-8")
(check (pi-provider-default-model "deepseek") => "deepseek-v4-pro")
(check (pi-provider-default-model "glm") => "glm-5.3")
(check (pi-provider-default-model "google") => "gemini-3.8-flash")
(check (pi-provider-default-model "kimi-coding") => "kimi-for-coding")
(check (pi-provider-default-model "minimax") => "MiniMax-M3.1-Flash")
(check (pi-provider-default-model "minimax-cn") => "MiniMax-M3.1-Flash")
(check (pi-provider-default-model "zai-coding-cn") => "glm-5.3")
;; 8.2 默认模型在合并目录内检查（除 minimax-cn 触发回退兜底外均直接在目录内）
(let* ((agent-dir (pi-get-agent-dir)) (catalog (pi-build-models-catalog agent-dir)))
  (for-each
    (lambda (prov)
      (let* ((def-model (pi-provider-default-model prov))
             (models (pi-catalog-provider-models catalog prov))
             (ids
               (map (lambda (m) (cdr (assoc "id" m))) models)
             ) ;ids
            ) ;
        (check-true (pair? (member def-model ids)))
      ) ;let*
    ) ;lambda
    '("anthropic" "deepseek" "glm" "google" "kimi-coding" "minimax"
      "zai-coding-cn")
  ) ;for-each
  ;; minimax-cn 默认模型不在目录内，触发回退到目录首个模型 MiniMax-M2.7（兜底逻辑）
  (let ((prov-settings '(("default-provider" . "minimax-cn"))))
    (let-values (((p m) (pi-route-provider-and-model "minimax-cn" #f prov-settings catalog '()))
                ) ;
      (check p => "minimax-cn")
      (check m => "MiniMax-M2.7")
    ) ;let-values
  ) ;let
) ;let*

(check-report)

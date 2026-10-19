(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi bash)
  (liii pi anthropic input)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [050] 验收脚本：deepseek tool_use 参数解析 value-error 修复
;; 逐条对应 tools/pi/devel/050.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 单文件行数 <= 300 行硬约束
;;   3) 单元测试验收（input 层 4 个新测试 + 既有修改测试）
;;   4) 函数级根因修复断言（多形态 input 兼容与 JSON 序列化对称性）
;;   5) deepseek 真实冒烟（--provider deepseek --tools bash 工具调用闭环验证）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-050-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
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

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 120)
  =>
  0
) ;check

(display "=== 验收点 2: 任务 050 相关文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/liii/pi/anthropic/input.scm"
               "tools/pi/liii/pi/anthropic/core.scm"
               "tools/pi/liii/pi/anthropic/stream-parse.scm"
               "tools/pi/liii/pi/models/meta.scm" "tools/pi/liii/pi/event.scm"
               "tools/pi/devel/050.scm")
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

(display "=== 验收点 3: 相关单元测试（input 层 4 个新测试 + 3 个既有修改测试） ===\n"
) ;display
(check (%run-ok? "t-norm"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic/input/anthropic-normalize-input-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-args"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic/input/anthropic-parse-tool-arguments-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-blk"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic/input/anthropic-assistant-blk-to-anthropic-blk-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-stream"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic/stream-parse/make-anthropic-stream-decoder-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-ant-msgs"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic-build-messages-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-ant-resp"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/anthropic-parse-response-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-event"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/pi-serialize-event-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: 函数级 input 归一化与序列化对称性验证 ===\n"
) ;display
;; 4.1 空参数归一化为 (quote (())) 且序列化为 {}
(check (anthropic-normalize-input '()) => '(()))
(check
  (json->string (list (cons "input" (anthropic-normalize-input '()))))
  =>
  "{\"input\":{}}"
) ;check
(check
  (json->string
    (list (cons "input" (anthropic-normalize-input '(()))))
  ) ;json->string
  =>
  "{\"input\":{}}"
) ;check
(check
  (json->string (list (cons "input" (anthropic-normalize-input #f))))
  =>
  "{\"input\":{}}"
) ;check
(check
  (json->string (list (cons "input" (anthropic-normalize-input 'null))))
  =>
  "{\"input\":{}}"
) ;check
(check
  (json->string (list (cons "input" (anthropic-normalize-input "{}"))))
  =>
  "{\"input\":{}}"
) ;check

;; 4.2 畸形 entry 归一化兼容（绝不能出现单元素 list 导致 value-error）
(let ((res (anthropic-normalize-input '(("path")))))
  (check (json->string (list (cons "input" res))) => "{\"input\":{\"path\":{}}}")
) ;let

;; 4.3 tool arguments 解析：空值/'() -> '()，JSON 字符串 -> alist
(check (anthropic-parse-tool-arguments '()) => '())
(check (anthropic-parse-tool-arguments '(())) => '())
(check (anthropic-parse-tool-arguments "{}") => '())
(let ((parsed (anthropic-parse-tool-arguments "{\"command\":\"echo ok\"}")))
  (check (cdr (assoc "command" parsed)) => "echo ok")
) ;let

(display "=== 验收点 5: deepseek 真实冒烟（差距 #11 修复验证） ===\n"
) ;display
(let* ((res (%run-capture "ds-smoke"
              "timeout 60 bin/gf pi --provider deepseek --tools bash '请使用 bash 工具运行 echo 050_SMOKE_TEST_OK'"
              90
            ) ;%run-capture
       ) ;res
       (rc (car res))
       (out (cdr res))
      ) ;
  (check rc => 0)
  (check-true (string-contains? out "050_SMOKE_TEST_OK"))
) ;let*

(check-report)

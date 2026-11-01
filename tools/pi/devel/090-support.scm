;; [090] 验收脚本的辅助定义（供 tools/pi/devel/090.scm 通过 (load ...) 使用）
;; 内容：外部命令执行封装、门禁检查与行数约束检查。

(define (%run-ok? tag cmd timeout)
  (let* ((tmp (string-append "tmp-090-" tag ".txt"))
         (ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                :stdin   'null
                :timeout timeout
              ) ;run
         ) ;ret
        ) ;
    (when (file-exists? tmp)
      (delete-file tmp)
    ) ;when
    ret
  ) ;let*
) ;define

(define (%run-output tag cmd timeout)
  (let* ((tmp (string-append "tmp-090-out-" tag ".txt"))
         (ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                :stdin   'null
                :timeout timeout
              ) ;run
         ) ;ret
         (out
           (if (file-exists? tmp)
             (let ((text (path-read-text (path tmp))))
               (delete-file tmp)
               text
             ) ;let
             ""
           ) ;if
         ) ;out
        ) ;
    (cons ret out)
  ) ;let*
) ;define

(define (verify-090-gates!)
  (display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
  (check (%run-ok? "build" "xmake b goldfish" 300) => 0)
  (check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
  (check (%run-ok? "check" "bin/gf check" 120) => 0)
  (check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)
) ;define

(define (verify-090-line-limits!)
  (display "=== 验收点 2: 任务 090 涉及的代码文件行数约束 (<= 300 行) ===\n"
  ) ;display
  (for-each
    (lambda (f)
      (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
        (check-true (<= lines 300))
      ) ;let
    ) ;lambda
    '("tools/pi/liii/pi/modes/tui/confirm.scm"
      "tools/pi/liii/pi/modes/tui/confirm/types.scm"
      "tools/pi/liii/pi/modes/tui/confirm/detect.scm"
      "tools/pi/liii/pi/modes/tui/confirm/render.scm"
      "tools/pi/liii/pi/modes/tui/state/confirm.scm"
      "tools/pi/liii/pi/modes/tui/state/types.scm"
      "tools/pi/liii/pi/modes/tui/state/pure.scm"
      "tools/pi/liii/pi/modes/tui/state/parse.scm"
      "tools/pi/liii/pi/modes/tui/loop/dispatch.scm"
      "tools/pi/liii/pi/modes/tui/runner.scm"
      "tools/pi/liii/pi/modes/tui/streaming.scm"
      "tools/pi/liii/pi/modes/tui/state.scm" "tools/pi/liii/pi/modes/tui.scm"
      "tools/pi/tests/liii/pi/modes/tui/confirm-test.scm"
      "tools/pi/devel/090.md" "tools/pi/devel/090.scm"
      "tools/pi/devel/090-support.scm")
  ) ;for-each
) ;define

(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
) ;import

(check-set-mode! 'report-failed)

;; [060] 验收脚本：(liii tui) 实现文件协议头迁移至 MIT
;; 逐条对应 tools/pi/devel/060.md 验收点：
;;   1) 门禁检查：构建 / fmt / check / doc
;;   2) 单文件行数 <= 300 行硬约束（全部 25 个实现文件）
;;   3) 25 个实现文件协议头准确包含 Mario Zechner 与 The Goldfish Scheme Authors
;;   4) 25 个实现文件准确包含 MIT License 全文关键语句
;;   5) 25 个实现文件均不再包含 Apache License, Version 2.0
;;   6) 33 个 TUI 测试文件保持无协议头规范（直接以 import 开头，不含许可证头）
;;   7) 仓库根 LICENSE 与其他库（如 goldfmt.scm, base.scm）保持 Apache 2.0
;;   8) 功能回归：顶级测试 tui-test.scm 与 059 验收脚本全绿

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-060-" tag ".txt")))
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

(define %tui-files
  '("goldfish/liii/tui.scm" "goldfish/liii/tui/box.scm"
    "goldfish/liii/tui/box/border.scm" "goldfish/liii/tui/box/box.scm"
    "goldfish/liii/tui/box/hstack.scm" "goldfish/liii/tui/box/hstack/width.scm"
    "goldfish/liii/tui/box/vstack.scm" "goldfish/liii/tui/component.scm"
    "goldfish/liii/tui/keys.scm" "goldfish/liii/tui/keys/parser.scm"
    "goldfish/liii/tui/keys/table.scm" "goldfish/liii/tui/loader.scm"
    "goldfish/liii/tui/loader/presets.scm"
    "goldfish/liii/tui/loader/spinner.scm" "goldfish/liii/tui/loader/state.scm"
    "goldfish/liii/tui/terminal.scm" "goldfish/liii/tui/terminal/protocol.scm"
    "goldfish/liii/tui/terminal/size.scm"
    "goldfish/liii/tui/terminal/standard.scm"
    "goldfish/liii/tui/terminal/virtual.scm"
    "goldfish/liii/tui/terminal/virtual/state.scm"
    "goldfish/liii/tui/text-width.scm" "goldfish/liii/tui/text-width/core.scm"
    "goldfish/liii/tui/text-width/truncate.scm"
    "goldfish/liii/tui/text-width/wrap.scm")
) ;define

(define %tui-tests
  '("tests/liii/tui-test.scm" "tests/liii/tui/box/border-style-test.scm"
    "tests/liii/tui/box/make-box-test.scm"
    "tests/liii/tui/box/make-hstack-test.scm"
    "tests/liii/tui/box/make-vstack-test.scm"
    "tests/liii/tui/box/nested-layout-test.scm"
    "tests/liii/tui/component/component-handle-input-test.scm"
    "tests/liii/tui/component/component-invalidate-test.scm"
    "tests/liii/tui/component/component-p-test.scm"
    "tests/liii/tui/component/component-render-test.scm"
    "tests/liii/tui/component/make-component-test.scm"
    "tests/liii/tui/component/make-spacer-test.scm"
    "tests/liii/tui/component/make-text-test.scm"
    "tests/liii/tui/component/make-truncated-text-test.scm"
    "tests/liii/tui/keys/parse-key-test.scm"
    "tests/liii/tui/keys/parse-keys-test.scm"
    "tests/liii/tui/loader/make-spinner-test.scm"
    "tests/liii/tui/loader/spinner-frames-test.scm"
    "tests/liii/tui/loader/spinner-render-test.scm"
    "tests/liii/tui/loader/spinner-status-test.scm"
    "tests/liii/tui/loader/spinner-text-test.scm"
    "tests/liii/tui/loader/spinner-tick-test.scm"
    "tests/liii/tui/loader/spinner-visible-test.scm"
    "tests/liii/tui/terminal/make-standard-terminal-test.scm"
    "tests/liii/tui/terminal/make-virtual-terminal-test.scm"
    "tests/liii/tui/terminal/terminal-cursor-test.scm"
    "tests/liii/tui/terminal/terminal-detect-size-test.scm"
    "tests/liii/tui/terminal/terminal-write-test.scm"
    "tests/liii/tui/terminal/virtual-terminal-resize-test.scm"
    "tests/liii/tui/text-width/display-width-test.scm"
    "tests/liii/tui/text-width/strip-ansi-test.scm"
    "tests/liii/tui/text-width/truncate-to-width-test.scm"
    "tests/liii/tui/text-width/wrap-text-test.scm")
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt" "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 实现文件数量与行数硬约束 (<= 300 行) ===\n"
) ;display
(check (length %tui-files) => 25)
(for-each
  (lambda (f)
    (check (file-exists? f) => #t)
    (let* ((text (path-read-text f))
           (lines (string-split text #\newline))
           (n (length lines))
          ) ;
      (check (<= n 300) => #t)
    ) ;let*
  ) ;lambda
  %tui-files
) ;for-each

(display "=== 验收点 3: 版权声明包含 Mario Zechner 与 The Goldfish Scheme Authors ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((text (path-read-text f)))
      (check (string-contains text "Copyright (c) 2025 Mario Zechner") => #t)
      (check (string-contains text "Copyright (C) 2024-2026 The Goldfish Scheme Authors")
        =>
        #t
      ) ;check
    ) ;let
  ) ;lambda
  %tui-files
) ;for-each

(display "=== 验收点 4: MIT License 关键授权语句完整包含 ===\n")
(for-each
  (lambda (f)
    (let ((text (path-read-text f)))
      (check (string-contains text "Permission is hereby granted, free of charge")
        =>
        #t
      ) ;check
      (check (string-contains text
               "without restriction, including without limitation the rights"
             ) ;string-contains
        =>
        #t
      ) ;check
      (check (string-contains text
               "The above copyright notice and this permission notice shall be included"
             ) ;string-contains
        =>
        #t
      ) ;check
      (check (string-contains text
               "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND"
             ) ;string-contains
        =>
        #t
      ) ;check
    ) ;let
  ) ;lambda
  %tui-files
) ;for-each

(display "=== 验收点 5: 均不再包含 Apache License, Version 2.0 ===\n")
(for-each
  (lambda (f)
    (let ((text (path-read-text f)))
      (check (string-contains text "Apache License, Version 2.0") => #f)
    ) ;let
  ) ;lambda
  %tui-files
) ;for-each

(display "=== 验收点 6: 测试文件无协议头规范（无许可证声明头） ===\n"
) ;display
(check (length %tui-tests) => 33)
(for-each
  (lambda (f)
    (check (file-exists? f) => #t)
    (let ((text (path-read-text f)))
      (check (string-contains text "Permission is hereby granted") => #f)
      (check (string-contains text "Apache License, Version 2.0") => #f)
      (check (string-starts? text "(import") => #t)
    ) ;let
  ) ;lambda
  %tui-tests
) ;for-each

(display "=== 验收点 7: 根 LICENSE 与非 tui 库保留 Apache 协议头 ===\n"
) ;display
(check (string-contains (path-read-text "goldfish/liii/goldfmt.scm")
         "Apache License, Version 2.0"
       ) ;string-contains
  =>
  #t
) ;check
(check (string-contains (path-read-text "goldfish/liii/subprocess.scm")
         "Apache License, Version 2.0"
       ) ;string-contains
  =>
  #t
) ;check
(check (string-contains (path-read-text "LICENSE") "Apache License") => #t)

(display "=== 验收点 8: 回归验证（tui-test.scm 与 059 验收脚本） ===\n"
) ;display
(check (%run-ok? "tui-test" "bin/gf tests/liii/tui-test.scm" 60) => 0)
(check (%run-ok? "059-test" "bin/gf tools/pi/devel/059.scm" 120) => 0)

(check-report)

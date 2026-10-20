(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii tui)
  (liii tui text-width)
  (liii tui loader)
  (liii tui loader presets)
  (liii tui loader spinner)
) ;import

(check-set-mode! 'report-failed)

;; [059] 验收脚本：(liii tui loader) 转轮动画与状态组件
;; 逐条对应 tools/pi/devel/059.md 验收点：
;;   1) 门禁检查：构建 / fmt / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 单函数单元测试全量通过（CLI 执行无头测试，耗时 < 10 秒）
;;   4) Spinner 预置样式与自定义帧解析
;;   5) 帧推进状态机与取模 (spinner-tick! / reset!)
;;   6) 提示文本、前缀与后缀动态更新与单行组合
;;   7) 显隐切换 (spinner-show! / spinner-hide!)
;;   8) 终态转换与完成图标 (✔ / ✖ / ⚠ / ■)
;;   9) 宽度自适应与超宽安全截断
;;   10) (liii tui) 顶级门面整合与异常类型拦截

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-059-" tag ".txt")))
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
(check (%run-ok? "fmt" "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 059 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/loader.scm"
               "goldfish/liii/tui/loader/presets.scm"
               "goldfish/liii/tui/loader/state.scm"
               "goldfish/liii/tui/loader/spinner.scm" "tests/liii/tui-test.scm"
               "tests/liii/tui/loader/spinner-frames-test.scm"
               "tests/liii/tui/loader/make-spinner-test.scm"
               "tests/liii/tui/loader/spinner-tick-test.scm"
               "tests/liii/tui/loader/spinner-text-test.scm"
               "tests/liii/tui/loader/spinner-visible-test.scm"
               "tests/liii/tui/loader/spinner-status-test.scm"
               "tests/liii/tui/loader/spinner-render-test.scm"
               "tools/pi/devel/059.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let* ((lines (string-split (path-read-text f) #\newline)) (n (length lines)))
        (check (<= n 300) => #t)
      ) ;let*
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 单函数单元测试全量通过 (CLI 执行) ===\n"
) ;display
(for-each
  (lambda (test-file)
    (check (%run-ok? "unit-test" (string-append "bin/gf " test-file) 30) => 0)
  ) ;lambda
  '("tests/liii/tui/loader/spinner-frames-test.scm"
    "tests/liii/tui/loader/make-spinner-test.scm"
    "tests/liii/tui/loader/spinner-tick-test.scm"
    "tests/liii/tui/loader/spinner-text-test.scm"
    "tests/liii/tui/loader/spinner-visible-test.scm"
    "tests/liii/tui/loader/spinner-status-test.scm"
    "tests/liii/tui/loader/spinner-render-test.scm" "tests/liii/tui-test.scm")
) ;for-each

(display "=== 验收点 4: Spinner 预置样式与自定义帧解析 ===\n")
(let ((p (spinner-presets)))
  (check (member 'dots p) => '(dots line pipe star bounce pulse ascii))
  (check (car (spinner-frames-resolve 'dots)) => "⠋")
  (check (car (spinner-frames-resolve 'line)) => "-")
  (check (car (spinner-frames-resolve 'pipe)) => "┤")
  (check (car (spinner-frames-resolve 'star)) => "✶")
  (check (car (spinner-frames-resolve 'bounce)) => "⠁")
  (check (car (spinner-frames-resolve 'pulse)) => "█")
  (check (car (spinner-frames-resolve 'ascii)) => "|")
  (check (spinner-frames-resolve '("A" "B")) => '("A" "B"))
) ;let

(display "=== 验收点 5: 帧推进状态机与取模 (spinner-tick! / reset!) ===\n"
) ;display
(let ((s (make-spinner "Work" :frames 'line)))
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
  (check (spinner-tick! s) => "\\")
  (check (spinner-frame-index s) => 1)
  (check (spinner-tick! s) => "|")
  (check (spinner-frame-index s) => 2)
  (check (spinner-tick! s) => "/")
  (check (spinner-frame-index s) => 3)
  (check (spinner-tick! s) => "-")
  (check (spinner-frame-index s) => 0)
  (spinner-tick! s)
  (check (spinner-frame-index s) => 1)
  (spinner-reset! s)
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
) ;let

(display "=== 验收点 6: 提示文本、前缀与后缀动态更新与单行组合 ===\n"
) ;display
(let ((s (make-spinner "Job" :frames 'line :prefix "[1] " :suffix " (0%)")))
  (check (component-render s 40) => '("[1] - Job (0%)"))
  (spinner-set-text! s "Updated")
  (spinner-set-prefix! s ">> ")
  (spinner-set-suffix! s " (50%)")
  (check (component-render s 40) => '(">> - Updated (50%)"))
  (spinner-set-prefix! s "")
  (spinner-set-suffix! s "")
  (spinner-set-text! s "")
  (check (component-render s 40) => '("-"))
) ;let

(display "=== 验收点 7: 显隐切换 (spinner-show! / spinner-hide!) ===\n")
(let ((s (make-spinner "Active" :frames 'line)))
  (check (spinner-visible? s) => #t)
  (check (component-render s 40) => '("- Active"))
  (spinner-hide! s)
  (check (spinner-visible? s) => #f)
  (check (component-render s 40) => '())
  (spinner-show! s)
  (check (spinner-visible? s) => #t)
  (check (component-render s 40) => '("- Active"))
) ;let

(display "=== 验收点 8: 终态转换与完成图标 (✔ / ✖ / ⚠ / ■) ===\n"
) ;display
(let ((s (make-spinner "Deploy" :frames 'line)))
  ;; 成功
  (spinner-success! s "Deploy succeeded")
  (check (spinner-status s) => 'success)
  (check (spinner-current-frame s) => "✔")
  (check (component-render s 40) => '("✔ Deploy succeeded"))
  ;; 终态下 tick! 不再递增帧索引
  (check (spinner-tick! s) => "✔")
  ;; 失败
  (spinner-fail! s "Deploy failed")
  (check (spinner-status s) => 'error)
  (check (spinner-current-frame s) => "✖")
  (check (component-render s 40) => '("✖ Deploy failed"))
  ;; 警告
  (spinner-warn! s "Deploy with warnings")
  (check (spinner-status s) => 'warning)
  (check (spinner-current-frame s) => "⚠")
  ;; 停止
  (spinner-stop! s)
  (check (spinner-status s) => 'stopped)
  (check (spinner-current-frame s) => "■")
  ;; reset! 恢复 spinning 状态
  (spinner-reset! s)
  (check (spinner-status s) => 'spinning)
  (check (spinner-current-frame s) => "-")
) ;let

(display "=== 验收点 9: 宽度自适应与超宽安全截断 ===\n")
(let* ((long-text "Very long loading message for testing terminal width truncation boundary"
       ) ;long-text
       (s (make-spinner long-text :frames 'line :prefix ">> "))
       (w 20)
       (lines (component-render s w))
      ) ;
  (check (length lines) => 1)
  (check (<= (display-width (car lines)) w) => #t)
  ;; 极窄宽度边界
  (check (component-render s 0) => '(""))
  (check (length (component-render s 1)) => 1)
) ;let*

(display "=== 验收点 10: (liii tui) 顶级门面整合与异常类型拦截 ===\n"
) ;display
(let ((l (make-loader "Facade check" :frames 'star)))
  (check (component? l) => #t)
  (check (loader? l) => #t)
  (check (spinner? l) => #t)
  (check (component-render l 40) => '("✶ Facade check"))
) ;let

(check-catch 'type-error (spinner-frames-resolve 'not-exist))
(check-catch 'type-error (make-spinner :frames 123))
(check-catch 'type-error (make-spinner :interval -5))
(check-catch 'type-error (spinner-set-text! "not-sp" "abc"))
(check-catch 'type-error (spinner-set-text! (make-spinner) 123))
(check-catch 'type-error (spinner-set-prefix! (make-spinner) #f))
(check-catch 'type-error (spinner-set-suffix! (make-spinner) #f))

(check-report)

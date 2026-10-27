(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; Bash 折叠保留最后 5 个视觉行，隐藏行提示及 Ctrl+O 文案与上游一致
(let* ((result "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-bash-tool-result-lines result 80 #f)))
  (check (car lines) => "... (3 earlier lines, ctrl+o to expand)")
  (check (cdr lines) => '("4" "5" "6" "7" "8"))
  (check (format-bash-tool-result-lines result 80 #t)
    => '("1" "2" "3" "4" "5" "6" "7" "8")))

;; 一个长逻辑行按终端宽度换为多个视觉行，折叠时不能只按换行符计数
(let ((lines (format-bash-tool-result-lines "abcdefghijklmnop" 8 #f)))
  (check (length lines) => 2)
  (check (car lines) => "abcdefgh")
  (check (cadr lines) => "ijklmnop"))

;; 渲染层对齐上游 getTextOutput + Text：剥 ANSI/OSC、删控制字符与 \r，TAB 展开为定宽 3 空格
(let ((lines (format-bash-tool-result-lines "a\tb\tc" 80 #f)))
  (check lines => '("a   b   c")))

;; 残留 ANSI（含 OSC）与 \r 不出现在渲染行中
(let ((lines (format-bash-tool-result-lines "\x1b]0;title\x07;\x1b[32mok\x1b[0m\r\ndone" 80 #f)))
  (check lines => '("ok" "done")))

;; 控制字符被删除
(let ((lines (format-bash-tool-result-lines "a\x01;b" 80 #f)))
  (check lines => '("ab")))

;; 折叠态尾部处理：500 行大输出只显示最后 5 个视觉行（避免每帧全文 wrap 能CPU飕升）
(let* ((big (string-join (map number->string (iota 500 1)) "\n"))
       (lines (format-bash-tool-result-lines big 80 #f)))
  (check (length lines) => 6)
  (check (car lines) => "... (495 earlier lines, ctrl+o to expand)")
  (check (cadr lines) => "496")
  (check (last lines) => "500"))

;; 超宽行 wrap 后折叠：显示最后 5 个视觉行，hidden 计逻辑行数（尾部处理语义）
(let* ((text (string-append "1\n2\n3\n4\n5\n6\n7\n" "abcdefghijklmnop"))
       (lines (format-bash-tool-result-lines text 8 #f)))
  ;; 尾部 5 逻辑行 = 4..7+超宽行，超宽行 wrap 成 2 视觉行 → M=6 > 5，取最后 5
  (check lines => '("... (3 earlier lines, ctrl+o to expand)"
                   "5" "6" "7" "abcdefgh" "ijklmnop")))

(check-report)

(import (scheme base)
  (liii check)
  (liii list)
  (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; 简单单行
(check (format-tool-result-lines "bash" "Hello World") => '("Hello World"))

;; 短多行（<= 5 行完整保留）
(let ((lines (format-tool-result-lines "bash" "line1\nline2\nline3")))
  (check lines => '("line1" "line2" "line3")))

;; bash 超长（如 8 行）：默认保留尾部 5 行，并在首部追加折叠提示
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "bash" text :max-lines 5)))
  (check (length lines) => 6)
  (check (car lines) => "... (3 earlier lines, ctrl+o to expand)")
  (check (cdr lines) => '("4" "5" "6" "7" "8")))

;; read/grep 等超长（如 8 行）：默认保留头部 5 行，尾部追加折叠提示
;; 提示文案对齐上游 createResultFallback："... (N more lines, ctrl+o to expand)"
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "read" text :max-lines 5)))
  (check (length lines) => 6)
  (check (list-ref lines 5) => "... (3 more lines, ctrl+o to expand)")
  (check (take lines 5) => '("1" "2" "3" "4" "5")))

;; 结构化结果（如 pi-tool-result 或 inlet 包含 content）提取测试
(let* ((mock-inlet (inlet 'is-error #f 'content #(((type . "text") (text . "ddd\n")))))
       (lines (format-tool-result-lines "bash" mock-inlet)))
  (check lines => '("ddd")))

(check-report)

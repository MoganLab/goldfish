;; e2e-commonmark
;; CommonMark 0.31.2 端到端规范回归测试（Example 1-652 及补充样例）。
;; 每个资源默认执行双向检查：正向（Markdown -> md-stem 等于资源）
;; 与反向语义往返（md-stem -> Markdown -> md-stem 等于资源）；
;; 少数例外在 e2e-directions.scm 集中登记方向与原因。
;; 资源遍历辅助与 e2e-extensions-test.scm 一致，方向定义只维护一份。
;;
;; 语法
;; ----
;; bin/gf test cmark/tests/liii/cmark/e2e-commonmark-test.scm
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; unspecified
;; 验证 tests/resources/markdown/ 下全部 CommonMark 样例的双向转换结果。

(import (scheme base))
(import (scheme file))
(import (liii check))
(import (liii cmark))
(import (liii cmark stem-reader))
(import (liii unicode))

;; 方向清单（数据）只维护一份：tests/liii/cmark/e2e-directions.scm
(load "tests/liii/cmark/e2e-directions.scm")

(check-set-mode! 'report-failed)

(define (read-file-string path)
  (call-with-input-file path
    (lambda (port)
      (let loop
        ((chars '()))
        (let ((ch (read-char port)))
          (if (eof-object? ch) (list->string (reverse chars)) (loop (cons ch chars)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; 资源方向：both / forward-only / reverse-only；未登记默认 both
(define (resource-direction basename)
  (let ((entry (assoc basename e2e-direction-entries string=?)))
    (if entry (cadr entry) 'both)
  ) ;let
) ;define

;; 按方向检查单个资源：
;; - both：正向相等 + 反向语义往返
;; - forward-only：只检查正向相等
;; - reverse-only：只检查反向语义往返
(define (check-resource-case basename)
  (let* ((base (string-append "tests/resources/markdown/" basename))
         (markdown (read-file-string (string-append base ".md")))
         (expected (call-with-input-file (string-append base ".md.stem") read-md-stem))
         (direction (resource-direction basename))
        ) ;
    (when (memq direction '(both forward-only))
      (check (markdown->md-stem markdown) => expected)
    ) ;when
    (when (memq direction '(both reverse-only))
      (check (markdown->md-stem (md-stem->markdown expected)) => expected)
    ) ;when
  ) ;let*
) ;define

(define (format-index n width)
  (let ((s (number->string n)))
    (if (< (string-length s) width)
      (string-append (make-string (- width (string-length s)) #\0) s)
      s
    ) ;if
  ) ;let
) ;define

(define (check-example-range dir width from to)
  (let loop
    ((i from))
    (when (<= i to)
      (check-resource-case (string-append dir "/Example" (format-index i width)))
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;define

(check-example-range "2_Preliminaries/2.1_Tabs" 2 1 11)
(check-example-range "2_Preliminaries/2.2_Backslash_escapes" 2 12 24)
(check-example-range "3_Blocks_and_inlines/3.1_Precedence" 2 42 42)
(check-example-range "4_Leaf_blocks/4.1_Thematic_breaks" 2 43 61)
(check-resource-case "4_Leaf_blocks/4.1_Thematic_breaks/Extra01")
(check-resource-case "4_Leaf_blocks/4.1_Thematic_breaks/Extra02")
(check-example-range "4_Leaf_blocks/4.2_ATX_headings" 2 62 79)
(check-example-range "4_Leaf_blocks/4.3_Setext_headings" 2 80 106)
(check-example-range "4_Leaf_blocks/4.4_Indented_code_blocks" 3 107 118)
(check-example-range "4_Leaf_blocks/4.5_Fenced_code_blocks" 3 119 147)
(check-example-range "4_Leaf_blocks/4.6_HTML_blocks" 3 148 191)
(check-resource-case "4_Leaf_blocks/4.6_HTML_blocks/Extra01")
(check-example-range "4_Leaf_blocks/4.7_Link_reference_definitions" 3 192 218)
(check-example-range "4_Leaf_blocks/4.8_Paragraphs" 3 219 226)
(check-example-range "4_Leaf_blocks/4.9_Blank_lines" 3 227 227)
(check-example-range "5_Container_blocks/5.1_Block_quotes" 3 228 252)
(check-resource-case "5_Container_blocks/5.1_Block_quotes/Extra01")
(check-resource-case "5_Container_blocks/5.1_Block_quotes/Extra02")
(check-example-range "5_Container_blocks/5.2_List_items" 3 253 300)
(check-example-range "5_Container_blocks/5.3_Lists" 3 301 326)
(check-example-range "6_Inlines" 3 327 327)
(check-example-range "6_Inlines/6.1_Code_spans" 3 328 349)
(check-example-range "6_Inlines/6.2_Emphasis_and_strong_emphasis" 3 350 481)
(check-example-range "6_Inlines/6.3_Links" 3 482 571)
(check-example-range "6_Inlines/6.4_Images" 3 572 593)
(check-example-range "6_Inlines/6.5_Autolinks" 3 594 612)
(check-example-range "6_Inlines/6.6_Raw_HTML" 3 613 632)
(check-resource-case "6_Inlines/6.6_Raw_HTML/Extra01")
(check-resource-case "6_Inlines/6.6_Raw_HTML/Extra02")
(check-resource-case "6_Inlines/6.6_Raw_HTML/Extra03")
(check-example-range "6_Inlines/6.7_Hard_line_breaks" 3 633 647)
(check-resource-case "6_Inlines/6.7_Hard_line_breaks/Extra01")
(check-resource-case "6_Inlines/6.7_Hard_line_breaks/Extra02")
(check-example-range "6_Inlines/6.8_Soft_line_breaks" 3 648 649)
(check-resource-case "6_Inlines/6.8_Soft_line_breaks/Extra01")
(check-example-range "6_Inlines/6.9_Textual_content" 3 650 652)

(check-report)

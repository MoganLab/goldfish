;; e2e-extensions
;; Markdown 数学公式、GFM/Goldfish 扩展与综合端到端回归测试。
;; 每个资源默认执行双向检查：正向（Markdown -> md-stem 等于资源）
;; 与反向语义往返（md-stem -> Markdown -> md-stem 等于资源）；
;; 少数例外在 e2e-directions.scm 集中登记方向与原因。
;; 资源遍历辅助与 e2e-commonmark-test.scm 一致，方向定义只维护一份。
;;
;; 语法
;; ----
;; bin/gf test cmark/tests/liii/cmark/e2e-extensions-test.scm
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; unspecified
;; 验证数学公式、表格、任务列表、删除线、裸链接、Alert、Emoji 与综合样例的双向转换结果。

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

(define (check-numbered-range prefix from to)
  (let loop
    ((i from))
    (when (<= i to)
      (let ((s (number->string i)))
        (check-resource-case (string-append prefix (if (< i 10) (string-append "0" s) s))
        ) ;check-resource-case
      ) ;let
      (loop (+ i 1))
    ) ;when
  ) ;let
) ;define

(check-numbered-range "7_Math/7.1_Dollars_inline/Math" 1 22)
(check-numbered-range "7_Math/7.2_Dollars_block/Math" 23 32)
(check-numbered-range "7_Math/7.3_Fenced_math/Math" 33 39)
(check-numbered-range "7_Math/7.4_Backslash_delimiters/Math" 40 45)
(check-numbered-range "7_Math/7.4_Backslash_delimiters/Math" 83 89)
(check-numbered-range "7_Math/7.5_Double_backslash/Math" 46 49)
(check-numbered-range "7_Math/7.6_Naked_latex/Math" 50 52)
(check-numbered-range "7_Math/7.7_Interaction/Math" 53 68)
(check-numbered-range "7_Math/7.9_Operators/Math" 81 82)

(check-numbered-range "8_Extensions/8.1_Tables/Table" 1 12)
(check-numbered-range "8_Extensions/8.2_Task_lists/Task" 1 12)
(check-numbered-range "8_Extensions/8.3_Strikeout/Strike" 1 12)
(check-numbered-range "8_Extensions/8.4_Bare_autolinks/Autolink" 1 12)
(check-numbered-range "8_Extensions/8.5_Alerts/Alert" 1 12)
(check-numbered-range "8_Extensions/8.6_Emoji/Emoji" 1 12)
(check-numbered-range "integration_" 1 14)

(check-report)

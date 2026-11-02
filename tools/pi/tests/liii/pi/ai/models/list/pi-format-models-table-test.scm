(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi ai models list)
  (liii pi ai models)
) ;import

(check-set-mode! 'report-failed)

;; 空结果且有搜索词
(check (pi-format-models-table '() "nonexistent")
  =>
  "No models matching \"nonexistent\"\n"
) ;check

;; 空结果且无搜索词
(check (pi-format-models-table '() #f) => "")

;; 有模型行数据时的表格格式化

(define mock-rows
  (list '((provider . "google")
          (model . "gemini-2.5-flash")
          (context . "1.0M")
          (max-out . "65.5K")
          (thinking . "yes")
          (images . "yes"))
    '((provider . "kimi-coding")
      (model . "k3")
      (context . "1.0M")
      (max-out . "131.1K")
      (thinking . "yes")
      (images . "yes"))
  ) ;list
) ;define

(define table-output (pi-format-models-table mock-rows))

;; 检查表头存在
(check-true (string-contains? table-output "on"))
(check-true (string-contains? table-output "provider"))
(check-true (string-contains? table-output "model"))
(check-true (string-contains? table-output "context"))
(check-true (string-contains? table-output "max-out"))
(check-true (string-contains? table-output "thinking"))
(check-true (string-contains? table-output "images"))

;; 检查行数据存在
(check-true (string-contains? table-output "gemini-2.5-flash"))
(check-true (string-contains? table-output "k3"))

;; 验证行数（表头 + 2 数据行，末尾有换行共 3 行）
(let ((lines (string-split (string-trim-right table-output #\newline) "\n")))
  (check (length lines) => 3)
) ;let

;; 混合激活与未激活的行

(define mixed-rows
  (list '((provider . "google")
          (model . "gemini-2.5-flash")
          (context . "1.0M")
          (max-out . "65.5K")
          (thinking . "yes")
          (images . "yes")
          (enabled . #t))
    '((provider . "kimi-coding")
      (model . "k3")
      (context . "1.0M")
      (max-out . "131.1K")
      (thinking . "yes")
      (images . "yes")
      (enabled . #f))
  ) ;list
) ;define

;; 无色模式测试（默认或 color? 为 #f）
(let ((table-plain (pi-format-models-table mixed-rows #f)))
  ;; 1. 不包含任何 ANSI 转义序列
  (check-false (string-contains? table-plain "\x1b;"))
  ;; 2. 状态列标记 ● 和 × 存在
  (check-true (string-contains? table-plain "●"))
  (check-true (string-contains? table-plain "×"))
  ;; 3. 验证表头与数据行结构
  (let ((lines (string-split (string-trim-right table-plain #\newline) "\n")))
    (check (length lines) => 3)
    (check-true (string-starts? (list-ref lines 0) "on"))
    (check-true (string-starts? (list-ref lines 1) "●"))
    (check-true (string-starts? (list-ref lines 2) "×"))
  ) ;let
) ;let

;; 有色模式测试（color? 为 #t）
(let ((table-color (pi-format-models-table mixed-rows #t))
      (table-plain (pi-format-models-table mixed-rows #f))
     ) ;
  ;; 1. 包含 SGR 32（绿色前景）、SGR 2（暗显）与 SGR 0（重置）
  (check-true (string-contains? table-color "\x1b;[32m"))
  (check-true (string-contains? table-color "\x1b;[2m"))
  (check-true (string-contains? table-color "\x1b;[0m"))
  ;; 2. 已激活行包含绿色 ●：\x1b;[32m●\x1b;[0m
  (check-true (string-contains? table-color "\x1b;[32m●\x1b;[0m"))
  ;; 3. 未激活行包含暗显 ×：\x1b;[2m×\x1b;[0m 以及暗显整行文字
  (check-true (string-contains? table-color "\x1b;[2m×\x1b;[0m"))
  (let ((lines (string-split (string-trim-right table-color #\newline) "\n")))
    (check (length lines) => 3)
    ;; 未激活行整行暗显（除标记外也由 \x1b;[2m 包裹）
    (check-true (string-contains? (list-ref lines 2) "\x1b;[2m"))
    (check-true (string-ends? (list-ref lines 2) "\x1b;[0m"))
  ) ;let

  ;; 4. 列宽对齐不受转义序列影响：剥离 ANSI 码后与无色输出完全一致
  (check (strip-ansi table-color) => table-plain)
) ;let

(check-report)

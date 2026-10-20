(import (scheme base)
        (liii check)
        (liii string)
        (liii pi models list)
        (liii pi models))

(check-set-mode! 'report-failed)

;; 空结果且有搜索词
(check (pi-format-models-table '() "nonexistent") => "No models matching \"nonexistent\"\n")

;; 空结果且无搜索词
(check (pi-format-models-table '() #f) => "")

;; 有模型行数据时的表格格式化
(define mock-rows
  (list
    '((provider . "google")
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
      (images . "yes"))))

(define table-output (pi-format-models-table mock-rows))

;; 检查表头存在
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
  (check (length lines) => 3))

(check-report)

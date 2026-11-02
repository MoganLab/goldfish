(import (scheme base)
        (liii check)
        (liii tui text-width)
        (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 1. 单行无 ANSI 选区反显包裹（SGR 7 与 27）
(let* ((line "hello world")
       (res (highlight-selection-line line 2 4)))
  ;; 视觉内容一致
  (check (strip-ansi res) => "hello world")
  ;; 包含 SGR 7 与 SGR 27
  (check-true (string? res))
  (check res => "he\x1b;[7mllo\x1b;[27m world"))

;; 2. 选区延伸至行尾
(let* ((line "hello")
       (res (highlight-selection-line line 2 10)))
  (check (strip-ansi res) => "hello")
  (check res => "he\x1b;[7mllo\x1b;[27m"))

;; 3. 原有 ANSI 颜色代码与反显嵌套
(let* ((line "\x1b;[32mhello\x1b;[0m world")
       (res (highlight-selection-line line 2 7)))
  (check (strip-ansi res) => "hello world")
  ;; 原有的 32m 颜色保留，并在选中区域注入反显
  (check res => "\x1b;[32mhe\x1b;[7mllo\x1b;[0m\x1b;[7m wo\x1b;[27mrld"))

;; 4. 多行选区包裹：首行、中间行、末行与未选中行
(let* ((lines (list "line 0"
                    "line 1"
                    "line 2"
                    "line 3"))
       (sel (make-selection-range '(2 . 1) '(3 . 2) #t))
       (res (highlight-selection-lines lines sel)))
  ;; line 0 不受影响
  (check (list-ref res 0) => "line 0")
  ;; line 1 从列 2 选到行末
  (check (list-ref res 1) => "li\x1b;[7mne 1\x1b;[27m")
  ;; line 2 从行首选到列 3
  (check (list-ref res 2) => "\x1b;[7mline\x1b;[27m 2")
  ;; line 3 不受影响
  (check (list-ref res 3) => "line 3"))

;; 5. 未激活选区不修改任何行
(let* ((lines (list "hello" "world"))
       (sel (make-selection-range '(0 . 0) '(3 . 0) #f)))
  (check (highlight-selection-lines lines sel) => lines))

(check-report)

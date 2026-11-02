;; highlight-selection-line
;; 为单行文本在指定列区间注入反显（ANSI 选区高亮）控制码。
;;
;; 语法
;; ----
;; (highlight-selection-line line c1 c2)
;;
;; 参数
;; ----
;; line : string?
;; 原始行文本（可能含 ANSI 码）。
;; c1 : integer?
;; 起始视觉列（含）。
;; c2 : (or integer? #f)
;; 结束视觉列（含），若为 #f 则延伸至行尾。
;;
;; 返回值
;; -----
;; string?
;; 注入反显控制序列后的文本。

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii pi code modes interactive selection)
) ;import

(check-set-mode! 'report-failed)

;; 1. 单行无 ANSI 选区反显包裹（SGR 7 与 27）
(let* ((line "hello world") (res (highlight-selection-line line 2 4)))
  ;; 视觉内容一致
  (check (strip-ansi res) => "hello world")
  ;; 包含 SGR 7 与 SGR 27
  (check-true (string? res))
  (check res => "he\x1b;[7mllo\x1b;[27m world")
) ;let*

;; 2. 选区延伸至行尾
(let* ((line "hello") (res (highlight-selection-line line 2 10)))
  (check (strip-ansi res) => "hello")
  (check res => "he\x1b;[7mllo\x1b;[27m")
) ;let*

;; 3. 原有 ANSI 颜色代码与反显嵌套
(let* ((line "\x1b;[32mhello\x1b;[0m world")
       (res (highlight-selection-line line 2 7))
      ) ;
  (check (strip-ansi res) => "hello world")
  ;; 原有的 32m 颜色保留，并在选中区域注入反显
  (check res => "\x1b;[32mhe\x1b;[7mllo\x1b;[0m\x1b;[7m wo\x1b;[27mrld")
) ;let*

(check-report)

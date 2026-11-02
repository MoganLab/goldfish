(import (scheme base)
        (liii check)
        (liii tui terminal)
        (liii tui clipboard))

(check-set-mode! 'report-failed)

;; 1. OSC 52 转义序列构造测试
(check (osc52-sequence "hello") => "\x1b;]52;c;aGVsbG8=\x07")
(check (osc52-copy-sequence "hello") => "\x1b;]52;c;aGVsbG8=\x07")
(check (osc52-sequence "hello world\n") => "\x1b;]52;c;aGVsbG8gd29ybGQK\x07")

;; 中文 UTF-8 内容编码
(let ((seq (osc52-sequence "中文测试")))
  (check-true (string? seq))
  (check (substring seq 0 7) => "\x1b;]52;c;")
  (check (substring seq (- (string-length seq) 1) (string-length seq)) => "\x07"))

;; 空字符串
(check (osc52-sequence "") => "\x1b;]52;c;\x07")

;; 2. virtual-terminal 写入验证
(let ((term (make-virtual-terminal :width 80 :height 24)))
  (clipboard-copy! term "test copy")
  (check-true (pair? (member (osc52-sequence "test copy") (virtual-terminal-raw term)))))

;; 3. 原生剪贴板调用鲁棒性验证（返回布尔值，不抛异常）
(check-true (boolean? (native-clipboard-copy! "test copy")))

(check-report)

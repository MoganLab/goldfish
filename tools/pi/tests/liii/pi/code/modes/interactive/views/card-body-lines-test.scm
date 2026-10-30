(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views wrap)
) ;import

(check-set-mode! 'report-failed)

;; 内容宽度 = 终端宽度 - 左右留白，至少保留 1 列
(check (%card-content-width 40 0 0) => 40)
(check (%card-content-width 40 1 1) => 38)
(check (%card-content-width 40 1 0) => 39)
(check (%card-content-width 2 1 1) => 1)
(check (%card-content-width 0 0 0) => 1)

;; 折行：按内容宽度切分，每段交给 style-fn 生成最终行
(check (%card-body-lines "hello world" 20 0 0 (lambda (seg) (list seg)))
  => '("hello world")
) ;check
(check (%card-body-lines "hello world foo" 11 0 0 (lambda (seg) (list seg)))
  => '("hello world" "foo")
) ;check

;; 左右留白参与内容宽度计算，超宽内容在留白内折行
(check (%card-body-lines "abcdefghij" 8 1 1 (lambda (seg) (list seg)))
  => '("abcdef" "ghij")
) ;check

;; 多行文本保持段落结构（空行保留）
(check (%card-body-lines "a\n\nb" 20 0 0 (lambda (seg) (list seg)))
  => '("a" "" "b")
) ;check

;; style-fn 逐段生成最终行，可自行加前缀
(check (%card-body-lines "abcdefghij" 8 1 1
            (lambda (seg) (list (string-append " [" seg "]"))))
  => '(" [abcdef]" " [ghij]")
) ;check

;; 空文本折行后仍是一行空行
(check (%card-body-lines "" 20 0 0 (lambda (seg) (list seg)))
  => '("")
) ;check

(check-report)

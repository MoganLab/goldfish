(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii unicode)
  (liii tui)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

(define (%nums-text n)
  (string-join (map number->string (iota n 1)) "\n"))

;; 展开渲染：显示全部 35 行 + "(ctrl+o to collapse)"
(let* ((card (make-bash-card "seq 1 35"))
       (_ (bash-card-set-output! card (%nums-text 35)))
       (_2 (bash-card-set-complete! card 0))
       (_3 (bash-card-set-expanded! card #t))
       (lines (component-render card 80))
       (plain (map strip-ansi lines)))
  ;; 边框 + 头 + 空 + 35 输出 + 空 + 状态 + 边框 + 空 = 42 行
  (check (length lines) => 42)
  (check (list-ref plain 3) => " 1")
  (check (list-ref plain 37) => " 35")
  (check-true (string-contains? (list-ref plain 39) "(ctrl+o to collapse)")))

;; 展开状态切换：set-expanded! 两次往返
(let* ((card (make-bash-card "seq 1 30"))
       (_ (bash-card-set-output! card (%nums-text 30)))
       (_2 (bash-card-set-complete! card 0)))
  (bash-card-set-expanded! card #t)
  (check-true (bash-card-expanded? card))
  (bash-card-set-expanded! card #f)
  (check-false (bash-card-expanded? card))
  (let ((plain (map strip-ansi (component-render card 80))))
    ;; 折叠态恢复 20 行预览：26 = 30 - 20 + 1 为首行
    (check (list-ref plain 3) => " 11")
    (check-true (string-contains? (list-ref plain 24) "... 10 more lines (ctrl+o to expand)"))))

(check-report)

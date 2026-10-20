(import (liii check)
        (liii tui editor history))

(check-set-mode! 'report-failed)

;; make-history & 历史记录操作测试

;; 1. 初始状态
(let ((h (make-history 5)))
  (check (history? h) => #t)
  (check (history-entries h) => '())
  (check (history-max-size h) => 5)
  (check (history-index h) => #f)
  (check (history-draft h) => #f))

;; 2. 连续 push
(let ((h (make-history 3)))
  (history-push! h "cmd 1")
  (check (history-entries h) => '("cmd 1"))
  (history-push! h "cmd 2")
  (check (history-entries h) => '("cmd 2" "cmd 1"))
  ;; 重复相同内容不入栈
  (history-push! h "cmd 2")
  (check (history-entries h) => '("cmd 2" "cmd 1"))
  ;; 空字符串不入栈
  (history-push! h "")
  (check (history-entries h) => '("cmd 2" "cmd 1"))
  ;; 超出 max-size (3) 自动截断最旧记录
  (history-push! h "cmd 3")
  (history-push! h "cmd 4")
  (check (history-entries h) => '("cmd 4" "cmd 3" "cmd 2")))

;; 3. 浏览历史与草稿暂存恢复
(let ((h (make-history 10)))
  (history-push! h "first")
  (history-push! h "second")
  (history-push! h "third")

  ;; 当前输入框有草稿 "my draft"
  (check (history-prev h "my draft") => "third")
  (check (history-draft h) => "my draft")
  (check (history-index h) => 0)

  ;; 再次向上翻
  (check (history-prev h "third") => "second")
  (check (history-index h) => 1)

  (check (history-prev h "second") => "first")
  (check (history-index h) => 2)

  ;; 到顶后再按向上依然停留在顶端
  (check (history-prev h "first") => "first")
  (check (history-index h) => 2)

  ;; 向下回退
  (check (history-next h "first") => "second")
  (check (history-index h) => 1)

  (check (history-next h "second") => "third")
  (check (history-index h) => 0)

  ;; 回到底部，恢复草稿
  (check (history-next h "third") => "my draft")
  (check (history-draft h) => #f)
  (check (history-index h) => #f)

  ;; 已经脱离历史浏览模式，再次向下返回 #f
  (check (history-next h "my draft") => #f))

;; 4. 空历史记录
(let ((h (make-history)))
  (check (history-prev h "test") => #f)
  (check (history-next h "test") => #f))

(check-report)

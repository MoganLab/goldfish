(import (scheme base)
        (liii check)
        (liii string)
        (liii pi ai message))

(check-set-mode! 'report-failed)

;; ============================================================
;; pi-bash-execution->user-text：bashExecution 消息注入上下文的
;; 文本构造（对齐 pi 的 bashExecutionToText）
;; ============================================================

;; 1. 常规：命令 + 输出围栏 + 退出码 0 → 不附加退出码行
(check (pi-bash-execution->user-text "ls" "a\nb" 0)
  => "Ran `ls`\n```\na\nb\n```")

;; 2. 空输出 → (no output)
(check (pi-bash-execution->user-text "true" "" 0) => "Ran `true`\n(no output)")

;; 3. 非零退出码 → 附加退出码行
(check (pi-bash-execution->user-text "false" "boom" 1)
  => "Ran `false`\n```\nboom\n```\n\nCommand exited with code 1")

;; 4. 超时（退出码 -1）同样附加退出码行
(check (pi-bash-execution->user-text "sleep 5" "" -1)
  => "Ran `sleep 5`\n(no output)\n\nCommand exited with code -1")

;; 5. 退出码缺失（#f）→ 不附加退出码行
(check (pi-bash-execution->user-text "ls" "x" #f) => "Ran `ls`\n```\nx\n```")

;; 6. 多行输出保持原样
(check (pi-bash-execution->user-text "seq 1 3" "1\n2\n3" 0)
  => "Ran `seq 1 3`\n```\n1\n2\n3\n```")

(check-report)

;; 库组织对齐测试：tools/pi/liii/pi/code 根目录只保留 cli.scm/config.scm/modes.scm
;; 三个入口，其余实现归入 cli/、core/、modes/、utils/ 四层（对齐上游
;; packages/coding-agent 的 src/cli、src/core、src/modes、src/utils 组织）。
;; 本测试逐一导入各新库路径并验证代表性导出，防止结构回退。
(import (liii check)
  (liii raw-string)
  (liii pi code cli args)
  (liii pi code cli help)
  (liii pi code core compaction)
  (liii pi code core extensions)
  (liii pi code core failure)
  (liii pi code core prompt)
  (liii pi code core session-manager)
  (liii pi code core tools)
  (liii pi code modes interactive)
  (liii pi code utils git)
) ;import

(check-set-mode! 'report-failed)

;; cli/ 层：命令行参数解析与帮助文本
(check-true (procedure? parse-pi-args))
(check-true (string? PI_HELP_TEXT))

;; core/ 层：压缩、扩展机制、子进程失败策略、提示词、会话与工具
(check-true (procedure? pi-estimate-message-tokens))
(check-true (procedure? extension-registry?))
(check-true (procedure? pi-command-failure-detail))
(check-true (procedure? build-initial-prompt))
(check-true (procedure? get-session-dir))
(check-true (list? PI_DEFAULT_TOOL_NAMES))

;; modes/ 层：交互式 TUI
(check-true (procedure? pi-app?))

;; utils/ 层：git 变更文件解析
(check-true (procedure? pi-parse-changed-files))

(check-report)

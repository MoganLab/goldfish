(import (liii check)
  (liii string)
  (liii json)
  (liii pi ai api google-generative-ai)
  (liii pi code core prompt)
) ;import

(check-set-mode! 'report-failed)

;; pi-build-system-prompt
;; 构建包含激活工具、运行环境、上下文以及会话预算约束的完整系统提示词。

;; 1. 默认预算参数（session-timeout 600, max-turns 200）
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '())))
  (check-true (string-contains? sp "You are an expert coding assistant operating inside pi"))
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>"))
  (check-true (string-contains? sp "<session_budget>"))
  (check-true (string-contains? sp "Session Timeout: 600 seconds (hard wall-clock deadline)"))
  (check-true (string-contains? sp "Max Turns: 200 turns."))
  (check-true (string-contains? sp "Pacing: Pace your work accordingly."))
  (check-true (string-contains? sp "</session_budget>")))

;; 2. 自定义有限预算（例如 1800s, 50 轮）
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '() '() 1800 50)))
  (check-true (string-contains? sp "Session Timeout: 1800 seconds"))
  (check-true (string-contains? sp "Max Turns: 50 turns.")))

;; 3. 无硬性超时与无限轮次（session-timeout #f / 0, max-turns 0）
(let ((sp (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() 0 0)))
  (check-true (string-contains? sp "Session Timeout: None (no wall-clock deadline configured)."))
  (check-true (string-contains? sp "Max Turns: Unlimited (no turn limit configured).")))

(let ((sp (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() #f #f)))
  (check-true (string-contains? sp "Session Timeout: None (no wall-clock deadline configured)."))
  (check-true (string-contains? sp "Max Turns: Unlimited (no turn limit configured).")))

;; 默认工具集的完整说明与规则
(let ((sp (pi-build-system-prompt '("read" "bash" "edit" "write") "/test/cwd" #f #f '())))
  (check-true (string-contains? sp "<tools>\n- read: Read file contents"))
  (check-true (string-contains? sp "- bash: Execute bash commands"))
  (check-true (string-contains? sp "- edit: Make precise file edits"))
  (check-true (string-contains? sp "- write: Create or overwrite files"))
  (check-true (string-contains? sp
                "In addition to the tools above, you may have access to other custom tools"))
  (check-true (string-contains? sp "</tools>"))
  (check-true (string-contains? sp "<rules>\n- Use bash for file operations like ls, rg, find"))
  (check-true (string-contains? sp "- Be concise in your responses"))
  (check-true (string-contains? sp
                "- Show file paths clearly when working with files\n</rules>"))
  (check-false (string-contains? sp "<project_context>"))
  (check-false (string-contains? sp "<addendum>")))

;; 包含 grep 工具时不生成 "Use bash for file operations..."
(let ((sp (pi-build-system-prompt '("read" "bash" "edit" "write" "grep")
            "/test/cwd" #f #f '())))
  (check-false (string-contains? sp "Use bash for file operations like ls, rg, find"))
  (check-true (string-contains? sp "- grep: Search file contents"))
  (check-true (string-contains? sp "- Be concise in your responses")))

;; 包含 project_context 注入
(let* ((ctx '((path . "/abs/path/CLAUDE.md") (content . "Rule 1\nRule 2")))
       (sp (pi-build-system-prompt '("read" "bash") "/test/cwd" ctx #f '())))
  (check-true (string-contains? sp "<project_context>"))
  (check-true (string-contains? sp "Project-specific instructions and guidelines:"))
  (check-true (string-contains? sp
                "<project_instructions path=\"/abs/path/CLAUDE.md\">\nRule 1\nRule 2\n</project_instructions>"))
  (check-true (string-contains? sp "</project_context>")))

;; system-prompt-override 覆盖 preamble，且忽略 tools 和 rules
(let ((sp (pi-build-system-prompt '("read" "bash")
            "/test/cwd" #f "Custom system instruction" '())))
  (check-true (string-starts? sp "Custom system instruction"))
  (check-false (string-contains? sp "<tools>"))
  (check-false (string-contains? sp "<rules>"))
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>")))

;; append-system-prompts 注入 <addendum>
(let ((sp (pi-build-system-prompt '("read")
            "/test/cwd" #f #f '("Extra rule A" "Extra rule B"))))
  (check-true (string-contains? sp "<addendum>\nExtra rule A\n\nExtra rule B\n</addendum>")))

;; 4 字节 UTF-8 字符与 '<' 在系统提示词和请求体中的防回归测试
(let* ((ctx '((path . "/abs/path/CLAUDE.md")
              (content . "Rule with 🚀 and <xml>")))
       (sp (pi-build-system-prompt '("read" "bash")
             "/test/cwd" ctx #f '("Addendum 𠀀 <tag>")))
       (contents
         (gemini-build-contents (list (list (cons 'role "user") (cons 'content "🚀 <")))))
       (body (gemini-build-request-body contents sp '()))
       (json-str (json->string body)))
  (check-true (string-contains? sp "Rule with 🚀 and <xml>"))
  (check-true (string-contains? sp "Addendum 𠀀 <tag>"))
  (check-true (string-contains? json-str "🚀 <"))
  (check-true (string-contains? json-str "Rule with 🚀 and <xml>")))

(check-report)

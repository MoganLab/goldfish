(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check) (liii string) (liii path) (liii json) (liii pi))

(check-set-mode! 'report-failed)

(define REPO_ROOT
  (if (file-exists? "CLAUDE.md")
    "."
    (if (file-exists? "../../CLAUDE.md") "../.." ".")
  ) ;if
) ;define

(define TEST_FILE
  (path->string (path-join (path REPO_ROOT) (path "tools/pi/tests/fixtures/sample.txt")))
) ;define

;; pi-resolve-prompt-input
(check (pi-resolve-prompt-input "Hello prompt") => "Hello prompt")
(check (pi-resolve-prompt-input #f) => #f)

;; 读取现有文件
(let ((content (pi-resolve-prompt-input TEST_FILE)))
  (check-true (string? content))
  (check-true (string-contains? content "hello sample"))
) ;let

;; pi-load-project-context
(let ((ctx (pi-load-project-context REPO_ROOT)))
  (check-true (pair? ctx))
  (check-true (string-contains? (cdr (assoc 'path ctx)) "CLAUDE.md"))
  (check-true (string-contains? (cdr (assoc 'content ctx)) "Claude Code 工作规则")
  ) ;check-true
) ;let
(check (pi-load-project-context "/non-existent-dir-for-pi-test") => #f)

;; pi-build-system-prompt
;; 1. 默认工具集
(let ((sp (pi-build-system-prompt '("read" "bash" "edit" "write") "/test/cwd" #f #f '())
      ) ;sp
     ) ;
  (check-true (string-contains? sp "You are an expert coding assistant operating inside pi")
  ) ;check-true
  (check-true (string-contains? sp "<tools>\n- read: Read file contents"))
  (check-true (string-contains? sp "- bash: Execute bash commands"))
  (check-true (string-contains? sp "- edit: Make precise file edits"))
  (check-true (string-contains? sp "- write: Create or overwrite files"))
  (check-true (string-contains? sp
                "In addition to the tools above, you may have access to other custom tools"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp "</tools>"))
  (check-true (string-contains? sp
                "<rules>\n- Use bash for file operations like ls, rg, find"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp "- Be concise in your responses"))
  (check-true (string-contains? sp
                "- Show file paths clearly when working with files\n</rules>"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>"))
  (check-false (string-contains? sp "<project_context>"))
  (check-false (string-contains? sp "<addendum>"))
) ;let

;; 2. 包含 grep 工具时不生成 "Use bash for file operations..."
(let ((sp (pi-build-system-prompt '("read" "bash" "edit" "write" "grep")
            "/test/cwd"
            #f
            #f
            '()
          ) ;pi-build-system-prompt
      ) ;sp
     ) ;
  (check-false (string-contains? sp "Use bash for file operations like ls, rg, find")
  ) ;check-false
  (check-true (string-contains? sp "- grep: Search file contents"))
  (check-true (string-contains? sp "- Be concise in your responses"))
) ;let

;; 3. 包含 project_context 注入
(let* ((ctx '((path . "/abs/path/CLAUDE.md") (content . "Rule 1\nRule 2")))
       (sp (pi-build-system-prompt '("read" "bash") "/test/cwd" ctx #f '()))
      ) ;
  (check-true (string-contains? sp "<project_context>"))
  (check-true (string-contains? sp "Project-specific instructions and guidelines:")
  ) ;check-true
  (check-true (string-contains? sp
                "<project_instructions path=\"/abs/path/CLAUDE.md\">\nRule 1\nRule 2\n</project_instructions>"
              ) ;string-contains?
  ) ;check-true
  (check-true (string-contains? sp "</project_context>"))
) ;let*

;; 4. system-prompt-override 覆盖 preamble，且忽略 tools 和 rules
(let ((sp (pi-build-system-prompt '("read" "bash")
            "/test/cwd"
            #f
            "Custom system instruction"
            '()
          ) ;pi-build-system-prompt
      ) ;sp
     ) ;
  (check-true (string-starts? sp "Custom system instruction"))
  (check-false (string-contains? sp "<tools>"))
  (check-false (string-contains? sp "<rules>"))
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>"))
) ;let

;; 5. append-system-prompts 注入 <addendum>
(let ((sp (pi-build-system-prompt '("read")
            "/test/cwd"
            #f
            #f
            '("Extra rule A" "Extra rule B")
          ) ;pi-build-system-prompt
      ) ;sp
     ) ;
  (check-true (string-contains? sp "<addendum>\nExtra rule A\n\nExtra rule B\n</addendum>")
  ) ;check-true
) ;let

;; 6. 缺陷8防回归测试：4字节 UTF-8 字符（emoji、生僻字等）与 '<' 及各种组合的系统提示词与请求体
(let* ((ctx '((path . "/abs/path/CLAUDE.md")
              (content . "Rule with 🚀 and <xml>")))
       (sp (pi-build-system-prompt '("read" "bash")
             "/test/cwd"
             ctx
             #f
             '("Addendum 𠀀 <tag>")
           ) ;pi-build-system-prompt
       ) ;sp
       (contents
         (gemini-build-contents (list (list (cons 'role "user") (cons 'content "🚀 <")))
         ) ;gemini-build-contents
       ) ;contents
       (body (gemini-build-request-body contents sp '()))
       (json-str (json->string body))
      ) ;
  (check-true (string-contains? sp "Rule with 🚀 and <xml>"))
  (check-true (string-contains? sp "Addendum 𠀀 <tag>"))
  (check-true (string-contains? json-str "🚀 <"))
  (check-true (string-contains? json-str "Rule with 🚀 and <xml>"))
) ;let*

(check-report)

(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check) (liii string) (liii path) (liii pi) (liii pi tools))

(check-set-mode! 'report-failed)

(define test-file
  (if (file-exists? "liii/pi/cli.scm")
    "liii/pi/cli.scm"
    "tools/pi/liii/pi/cli.scm"
  ) ;if
) ;define

;; ============================================================
;; Tool Registry & Dispatcher
;; ============================================================

(check PI_DEFAULT_TOOL_NAMES => '("read" "bash" "edit" "write"))
(check (length PI_ALL_TOOL_NAMES) => 8)
(check (member "web_search" PI_ALL_TOOL_NAMES) => '("web_search"))

;; web_search 只在可选工具列表，不在默认工具集
(check-true (not (member "web_search" PI_DEFAULT_TOOL_NAMES)))

;; parse-tools-selection
(check (parse-tools-selection #f) => '("read" "bash" "edit" "write"))
(check (parse-tools-selection "read,grep") => '("read" "grep"))
(check (parse-tools-selection "+grep,-edit") => '("read" "bash" "write" "grep"))
(check (parse-tools-selection "+web_search")
  =>
  '("read" "bash" "edit" "write" "web_search")
) ;check
(check (parse-tools-selection "web_search") => '("web_search"))

;; exclude-tools-from-list
(check (exclude-tools-from-list '("read" "bash" "edit" "write") #f)
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "")
  =>
  '("read" "bash" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "bash,write,edit")
  =>
  '("read")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") " bash , edit ")
  =>
  '("read" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash" "edit" "write") "foo,bash")
  =>
  '("read" "edit" "write")
) ;check
(check (exclude-tools-from-list '("read" "bash") "read,bash") => '())

;; get-tool-definition
(let ((def (get-tool-definition "read")))
  (check-true (pi-tool-definition? def))
  (check (pi-tool-definition-name def) => "read")
  (check (cdr (assoc 'name def)) => "read")
  (check (cdr (assoc 'snippet def)) => "Read file contents")
) ;let

;; execute-tool-call: 未知工具
(let ((res (execute-tool-call "unknown_tool" '())))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #t)
  (check (cdr (assoc 'isError res)) => #t)
  (check (vector-ref (cdr (assoc 'content res)) 0)
    =>
    '((type . "text") (text . "Tool unknown_tool not found"))
  ) ;check
) ;let

;; execute-tool-call: 工具 throw 错误被捕获转成 isError: #t 的结果，不让整轮失败
(let ((res (execute-tool-call "read" '(("path" . "non_existent_file.xyz")))))
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #t)
  (check (cdr (assoc 'isError res)) => #t)
  (let ((text (cdr (assoc 'text (vector-ref (cdr (assoc 'content res)) 0)))))
    (check-true (string-contains? text "Path not found"))
  ) ;let
) ;let

;; execute-tool-call: 成功调用返回 isError: #f
(let ((res (execute-tool-call "read" (list (cons "path" test-file) (cons "limit" 1))))
     ) ;
  (check-true (pi-tool-result? res))
  (check (pi-tool-result-is-error res) => #f)
  (check (cdr (assoc 'isError res)) => #f)
) ;let

(check-report)

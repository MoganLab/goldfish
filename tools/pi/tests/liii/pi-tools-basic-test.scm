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
;; Tool: read
;; ============================================================

(let ((res (pi-execute-read (list (cons "path" test-file)))))
  (check-true (string? res))
  (check-true (string-contains? res "(define-library (liii pi cli)"))
) ;let

(let ((res (pi-execute-read (list (cons "path" test-file) (cons "limit" 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=3 to continue.")
  ) ;check-true
) ;let

(let ((res (pi-execute-read (list (cons "path" test-file) (cons "offset" 3) (cons "limit" 2))
           ) ;pi-execute-read
      ) ;res
     ) ;
  (check-true (string-contains? res "more lines in file. Use offset=5 to continue.")
  ) ;check-true
) ;let

(check-catch 'error
  (pi-execute-read (list (cons "path" test-file) (cons "offset" 999999)))
) ;check-catch

(check-catch 'error
  (pi-execute-read '(("path" . "non_existent_file_12345.txt")))
) ;check-catch

(check-catch 'error (pi-execute-read '(("path" . "tests"))))

(check-catch 'error
  (pi-execute-read (list (cons "path" test-file)) :timed-out? #t)
) ;check-catch

;; ============================================================
;; Tool: write
;; ============================================================

(let ((test-write-file "tests/liii/pi-write-fixture/sub/test.txt"))
  (let ((msg (pi-execute-write (list (cons "path" test-write-file) (cons "content" "hello write tool\nline 2"))
             ) ;pi-execute-write
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "hello write tool\nline 2")
    ) ;let
  ) ;let
  (let ((msg (pi-execute-write (list (cons "path" test-write-file) (cons "content" "overwritten content"))
             ) ;pi-execute-write
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "overwritten content")
    ) ;let
  ) ;let
  (delete-file test-write-file)
  (delete-file "tests/liii/pi-write-fixture/sub")
  (delete-file "tests/liii/pi-write-fixture")
) ;let

(check-catch 'error (pi-execute-write '(("path" . "foo.txt"))))
(check-catch 'error (pi-execute-write '(("content" . "hello"))))
(check-catch 'error
  (pi-execute-write '(("path" . "foo.txt") ("content" . "bar")) :timed-out? #t)
) ;check-catch

;; ============================================================
;; Tool: edit
;; ============================================================

(let ((test-edit-file "tests/liii/pi-edit-fixture.txt"))
  (pi-execute-write (list (cons "path" test-edit-file)
                      (cons "content" "line 1\nline 2: AAA\nline 3\nline 4: BBB\nline 5")
                    ) ;list
  ) ;pi-execute-write
  (let ((msg
          (pi-execute-edit
            (list (cons "path" test-edit-file)
              (cons "edits"
                (list (list (cons "oldText" "line 2: AAA") (cons "newText" "line 2: MODIFIED_A"))
                ) ;list
              ) ;cons
            ) ;list
          ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg
      =>
      (string-append "Successfully replaced 1 block(s) in " test-edit-file ".")
    ) ;check
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 2: MODIFIED_A"))
    ) ;let
  ) ;let
  (let ((msg
          (pi-execute-edit
            (list (cons "path" test-edit-file)
              (cons "edits"
                (list (list (cons "oldText" "line 1") (cons "newText" "line 1: FIRST"))
                  (list (cons "oldText" "line 4: BBB") (cons "newText" "line 4: SECOND"))
                ) ;list
              ) ;cons
            ) ;list
          ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg
      =>
      (string-append "Successfully replaced 2 block(s) in " test-edit-file ".")
    ) ;check
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 1: FIRST"))
      (check-true (string-contains? c "line 4: SECOND"))
    ) ;let
  ) ;let
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits"
          (list (list (cons "oldText" "NON_EXISTENT") (cons "newText" "NEW")))
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (delete-file test-edit-file)
) ;let

;; ============================================================
;; Tool: bash
;; ============================================================

(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'hello bash'")))))
  (check is-err => #f)
  (check (string-trim-both out) => "hello bash")
) ;let-values

(let-values (((out is-err) (pi-execute-bash '(("command"
                                               . "echo 'fail msg'; exit 42")))))
  (check is-err => #t)
  (check-true (string-contains? out "fail msg"))
  (check-true (string-contains? out "Command exited with code 42"))
) ;let-values

(check-catch 'error (pi-execute-bash '()))

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

;; get-tool-definition bash 支持默认与自定义超时 schema
(let* ((def (get-tool-definition "bash"))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check-true (pi-tool-definition? def))
  (check (cdr (assoc "description" t-prop)) => "Timeout in seconds (optional, defaults to 600s)"))

(let* ((def (get-tool-definition "bash" 300))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check (cdr (assoc "description" t-prop)) => "Timeout in seconds (optional, defaults to 300s)"))

(let* ((def (get-tool-definition "bash" :timeout 150))
       (schema (pi-tool-definition-parameters def))
       (props (cdr (assoc "properties" schema)))
       (t-prop (cdr (assoc "timeout" props))))
  (check (cdr (assoc "description" t-prop)) => "Timeout in seconds (optional, defaults to 150s)"))

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

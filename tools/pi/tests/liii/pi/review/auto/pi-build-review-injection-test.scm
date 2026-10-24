(import (liii check)
        (liii string)
        (liii pi ai message)
        (liii pi review auto)
) ;import

(check-set-mode! 'report-failed)

(define (file-entry path lang)
  (list (cons 'path path) (cons 'language lang))
) ;define

;; pi-build-review-injection
;; 空编辑列表不注入。
(check (pi-build-review-injection '()) => #f)

(let* ((files (list (file-entry "src/a.ts" "typescript")
                    (file-entry "src/b.py" "python")
                  ) ;list
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check-true (string-starts? block "\n\n## Code Review\n\nYou edited: src/a.ts, src/b.py."))
  (check-true (string-contains? block "Before proceeding, briefly verify:"))
  (check-true (string-contains? block "### Typescript"))
  (check-true (string-contains? block "### Python"))
  (check-true (string-ends? block "Fix any issues found, then continue."))
) ;let*

;; 多文件同语言时只出现一次清单
(let* ((files (list (file-entry "a.ts" "typescript")
                    (file-entry "b.ts" "typescript")
                    (file-entry "c.scm" "scheme")
                  ) ;list
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check (- (length (string-split block "### Typescript")) 1) => 1)
  (check (- (length (string-split block "### Scheme")) 1) => 1)
  (check-true (string-contains? block "You edited: a.ts, b.ts, c.scm."))
) ;let*

;; pi-edited-files-from-tool-calls
;; 只提取 write/edit 的 path，忽略其它工具；重复路径去重。
(define (tool-call name args)
  (list (cons "type" "toolCall")
    (cons "name" name)
    (cons "arguments" args)
  ) ;list
) ;define

(let* ((calls (list (tool-call "read" (list (cons "path" "read.ts")))
                    (tool-call "write" (list (cons "path" "a.ts") (cons "content" "x")))
                    (tool-call "edit" (list (cons "path" "b.py") (cons "edits" '())))
                    (tool-call "bash" (list (cons "command" "ls")))
                    (tool-call "write" (list (cons "path" "a.ts") (cons "content" "y")))
                  ) ;list
       ) ;calls
       (files (pi-edited-files-from-tool-calls calls))
      ) ;
  (check (length files) => 2)
  (check (cdr (assoc 'path (list-ref files 0))) => "a.ts")
  (check (cdr (assoc 'language (list-ref files 0))) => "typescript")
  (check (cdr (assoc 'path (list-ref files 1))) => "b.py")
  (check (cdr (assoc 'language (list-ref files 1))) => "python")
) ;let*

;; 兼容符号键与 file_path 别名；无 path 的调用忽略
(let* ((calls (list (list (cons 'type "toolCall")
                          (cons 'name "write")
                          (cons 'arguments (list (cons 'path "sym.scm")))
                    ) ;list
                    (list (cons 'type "toolCall")
                          (cons 'name "edit")
                          (cons 'arguments (list (cons "file_path" "alias.go")))
                    ) ;list
                    (list (cons 'type "toolCall")
                          (cons 'name "write")
                          (cons 'arguments '())
                    ) ;list
                  ) ;list
       ) ;calls
       (files (pi-edited-files-from-tool-calls calls))
      ) ;
  (check (length files) => 2)
  (check (cdr (assoc 'path (list-ref files 0))) => "sym.scm")
  (check (cdr (assoc 'language (list-ref files 0))) => "scheme")
  (check (cdr (assoc 'path (list-ref files 1))) => "alias.go")
  (check (cdr (assoc 'language (list-ref files 1))) => "go")
) ;let*

(check (pi-edited-files-from-tool-calls '()) => '())

;; 真实 <pi-tool-call-block> record 结构
(let* ((calls (list (pi-tool-call-block "c1" "write" '(("path" . "foo.scm")) :thought-signature "sig-1234567890")
                    (pi-tool-call-block "c2" "edit" '(("path" . "bar.py")))
                    (pi-tool-call-block "c3" "bash" '(("command" . "ls")))
                    (pi-tool-call-block "c4" "write" '(("path" . "foo.scm")))))
       (files (pi-edited-files-from-tool-calls calls)))
  (check (length files) => 2)
  (check (cdr (assoc 'path (list-ref files 0))) => "foo.scm")
  (check (cdr (assoc 'language (list-ref files 0))) => "scheme")
  (check (cdr (assoc 'path (list-ref files 1))) => "bar.py")
  (check (cdr (assoc 'language (list-ref files 1))) => "python"))

(check-report)

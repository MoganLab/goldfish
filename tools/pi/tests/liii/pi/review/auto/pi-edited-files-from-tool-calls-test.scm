(import (liii check)
        (liii pi ai message)
        (liii pi review auto)
) ;import

(check-set-mode! 'report-failed)

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

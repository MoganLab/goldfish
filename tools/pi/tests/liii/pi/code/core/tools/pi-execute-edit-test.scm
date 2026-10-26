(import (liii check) (liii path) (liii pi code core tools edit) (liii pi code core tools) (scheme file))

(check-set-mode! 'report-failed)

;; pi-execute-edit
;; 执行 edit 工具在文件中替换指定代码块。
(let* ((target (path->string (path-join (path-temp-dir) "test-pi-edit.txt")))
       (_ (call-with-output-file target (lambda (p) (display "hello old text" p))))
       (res (pi-execute-edit `(("path" . ,target)
                               ("edits" . ((("oldText" . "old") ("newText" . "new"))))))))
  (check-true (string? res))
  (delete-file target)
) ;let*

(let ((target (path->string (path-join (path-temp-dir) "test-pi-edit-err.txt"))))
  ;; 命中多次相同的 oldText 报错
  (pi-execute-write (list (cons "path" target) (cons "content" "dup\ndup\n")))
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "dup") (cons "newText" "new"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; 重叠修改报错
  (pi-execute-write (list (cons "path" target) (cons "content" "ABCDEFGH")))
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits"
          (list (list (cons "oldText" "ABCD") (cons "newText" "1234"))
            (list (cons "oldText" "CDEF") (cons "newText" "5678"))
          ) ;list
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; oldText 与 newText 相同报错
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits"
          (list (list (cons "oldText" "ABCDEFGH") (cons "newText" "ABCDEFGH")))
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; edits 列表为空报错
  (check-catch 'error
    (pi-execute-edit (list (cons "path" target) (cons "edits" '())))
  ) ;check-catch

  ;; oldText 为空字符串报错
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "") (cons "newText" "foo"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch

  ;; 模拟超时
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" target)
        (cons "edits" (list (list (cons "oldText" "ABC") (cons "newText" "XYZ"))))
      ) ;list
      :timed-out? #t
    ) ;pi-execute-edit
  ) ;check-catch
  (delete-file target)
) ;let

(check-report)

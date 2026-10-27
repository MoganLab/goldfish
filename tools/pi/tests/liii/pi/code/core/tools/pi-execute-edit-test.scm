(import (liii check) (liii string) (liii path) (liii pi code core tools edit) (liii pi code core tools) (scheme file))

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

;; 单块、多块替换与不匹配错误
(let ((test-edit-file "tests/liii/pi-edit-fixture.txt"))
  (pi-execute-write
    (list (cons "path" test-edit-file)
          (cons "content" "line 1\nline 2: AAA\nline 3\nline 4: BBB\nline 5")))
  (let ((msg (pi-execute-edit
               (list (cons "path" test-edit-file)
                     (cons "edits"
                       (list (list (cons "oldText" "line 2: AAA")
                                   (cons "newText" "line 2: MODIFIED_A"))))))))
    (check msg => (string-append "Successfully replaced 1 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 2: MODIFIED_A"))))
  (let ((msg (pi-execute-edit
               (list (cons "path" test-edit-file)
                     (cons "edits"
                       (list (list (cons "oldText" "line 1") (cons "newText" "line 1: FIRST"))
                             (list (cons "oldText" "line 4: BBB")
                                   (cons "newText" "line 4: SECOND"))))))))
    (check msg => (string-append "Successfully replaced 2 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 1: FIRST"))
      (check-true (string-contains? c "line 4: SECOND"))))
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
            (cons "edits"
              (list (list (cons "oldText" "NON_EXISTENT") (cons "newText" "NEW")))))))
  (delete-file test-edit-file))

(check-report)

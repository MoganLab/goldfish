(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii path)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

(define test-file
  (if (file-exists? "liii/goldpi.scm")
    "liii/goldpi.scm"
    "tools/pi/liii/goldpi.scm"
  ) ;if
) ;define

;; ============================================================
;; Tool: read
;; ============================================================

(let ((res (pi-execute-read (list (cons "path" test-file)))))
  (check-true (string? res))
  (check-true (string-contains? res "(define-library (liii goldpi)"))
) ;let

(let ((res (pi-execute-read (list (cons "path" test-file) (cons "limit" 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=3 to continue."))
) ;let

(let ((res (pi-execute-read (list (cons "path" test-file) (cons "offset" 3) (cons "limit" 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=5 to continue."))
) ;let

(check-catch 'error
  (pi-execute-read (list (cons "path" test-file) (cons "offset" 999999)))
) ;check-catch

(check-catch 'error
  (pi-execute-read '(("path" . "non_existent_file_12345.txt")))
) ;check-catch

(check-catch 'error
  (pi-execute-read '(("path" . "tests")))
) ;check-catch

(check-catch 'error
  (pi-execute-read (list (cons "path" test-file)) :timed-out? #t)
) ;check-catch

;; ============================================================
;; Tool: write
;; ============================================================

(let ((test-write-file "tests/liii/pi-write-fixture/sub/test.txt"))
  (let ((msg (pi-execute-write (list (cons "path" test-write-file)
                                 (cons "content" "hello write tool\nline 2")
                               ) ;list
             ) ;pi-execute-write
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully wrote to " test-write-file))
    (let ((content (pi-execute-read (list (cons "path" test-write-file)))))
      (check content => "hello write tool\nline 2")
    ) ;let
  ) ;let
  (let ((msg (pi-execute-write (list (cons "path" test-write-file)
                                 (cons "content" "overwritten content")
                               ) ;list
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
  (let ((msg (pi-execute-edit (list (cons "path" test-edit-file)
                                (cons "edits"
                                  (list (list (cons "oldText" "line 2: AAA")
                                          (cons "newText" "line 2: MODIFIED_A")
                                        ) ;list
                                  ) ;list
                                ) ;cons
                              ) ;list
             ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully replaced 1 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 2: MODIFIED_A"))
    ) ;let
  ) ;let
  (let ((msg (pi-execute-edit (list (cons "path" test-edit-file)
                                (cons "edits"
                                  (list
                                    (list (cons "oldText" "line 1")
                                      (cons "newText" "line 1: FIRST")
                                    ) ;list
                                    (list (cons "oldText" "line 4: BBB")
                                      (cons "newText" "line 4: SECOND")
                                    ) ;list
                                  ) ;list
                                ) ;cons
                              ) ;list
             ) ;pi-execute-edit
        ) ;msg
       ) ;
    (check msg => (string-append "Successfully replaced 2 block(s) in " test-edit-file "."))
    (let ((c (pi-execute-read (list (cons "path" test-edit-file)))))
      (check-true (string-contains? c "line 1: FIRST"))
      (check-true (string-contains? c "line 4: SECOND"))
    ) ;let
  ) ;let
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits" (list (list (cons "oldText" "NON_EXISTENT")
                                             (cons "newText" "NEW")
                                           ) ;list
                                     ) ;list
                       ) ;cons
                     ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (pi-execute-write (list (cons "path" test-edit-file) (cons "content" "dup\ndup\n")))
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits" (list (list (cons "oldText" "dup")
                                             (cons "newText" "new")
                                           ) ;list
                                     ) ;list
                       ) ;cons
                     ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (pi-execute-write (list (cons "path" test-edit-file) (cons "content" "ABCDEFGH")))
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits"
                         (list (list (cons "oldText" "ABCD") (cons "newText" "1234"))
                           (list (cons "oldText" "CDEF") (cons "newText" "5678"))
                         ) ;list
                       ) ;cons
                     ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits"
                         (list (list (cons "oldText" "ABCDEFGH") (cons "newText" "ABCDEFGH"))
                         ) ;list
                       ) ;cons
                     ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file) (cons "edits" '())))
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits"
                         (list (list (cons "oldText" "") (cons "newText" "foo")))
                       ) ;cons
                     ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file)
                       (cons "edits"
                         (list (list (cons "oldText" "ABC") (cons "newText" "XYZ")))
                       ) ;cons
                     ) ;list
      :timed-out? #t
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

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 'fail msg'; exit 42")))
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "fail msg"))
  (check-true (string-contains? out "Command exited with code 42"))
) ;let-values

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1)))
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo test")) :timed-out? #t)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 120 seconds"))
) ;let-values

(check-catch 'error (pi-execute-bash '()))

;; ============================================================
;; Tool: ls
;; ============================================================

(let ((out (pi-execute-ls '(("path" . ".")))))
  (check-true (string? out))
  (check-true (string-contains? out "tests/"))
) ;let

(let ((out (pi-execute-ls '(("path" . ".") ("limit" . 1)))))
  (check-true (string-contains? out "[1 entries limit reached. Use limit=2 for more]"))
) ;let

(let ((empty-dir "tests/liii/pi-empty-dir"))
  (path-mkdir (path empty-dir) :parents #t :exist-ok #t)
  (let ((out (pi-execute-ls (list (cons "path" empty-dir)))))
    (check out => "(empty directory)")
  ) ;let
  (delete-file empty-dir)
) ;let

(check-catch 'error (pi-execute-ls '(("path" . "non_existent_dir_99999"))))
(check-catch 'error
  (pi-execute-ls (list (cons "path" test-file)))
) ;check-catch
(check-catch 'error (pi-execute-ls '() :timed-out? #t))

;; ============================================================
;; Tool: find
;; ============================================================

;; 1. 查找 tools/pi 下匹配 *.scm 的文件
(let ((out (pi-execute-find '(("pattern" . "*.scm") ("path" . ".")))))
  (check-true (string? out))
  (check-true (string-contains? out "goldpi")))

;; 2. limit 生效与提示
(let ((out (pi-execute-find '(("pattern" . "*.scm") ("path" . ".") ("limit" . 1)))))
  (check-true (string-contains? out "[1 results limit reached]")))

;; 3. 无匹配文件
(let ((out (pi-execute-find '(("pattern" . "non_existent_pattern_*.xyz") ("path" . ".")))))
  (check out => "No files found matching pattern"))

;; 4. 路径不存在报错
(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . "non_existent_dir_99999"))))

;; 5. 超时路径测试
(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm")) :timed-out? #t))

(check-report)

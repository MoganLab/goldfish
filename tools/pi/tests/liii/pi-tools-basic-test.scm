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
  (pi-execute-write (list (cons "path" test-edit-file) (cons "content" "dup\ndup\n"))
  ) ;pi-execute-write
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits" (list (list (cons "oldText" "dup") (cons "newText" "new"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (pi-execute-write (list (cons "path" test-edit-file) (cons "content" "ABCDEFGH"))
  ) ;pi-execute-write
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits"
          (list (list (cons "oldText" "ABCD") (cons "newText" "1234"))
            (list (cons "oldText" "CDEF") (cons "newText" "5678"))
          ) ;list
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits"
          (list (list (cons "oldText" "ABCDEFGH") (cons "newText" "ABCDEFGH")))
        ) ;cons
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit (list (cons "path" test-edit-file) (cons "edits" '())))
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits" (list (list (cons "oldText" "") (cons "newText" "foo"))))
      ) ;list
    ) ;pi-execute-edit
  ) ;check-catch
  (check-catch 'error
    (pi-execute-edit
      (list (cons "path" test-edit-file)
        (cons "edits" (list (list (cons "oldText" "ABC") (cons "newText" "XYZ"))))
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

(let-values (((out is-err) (pi-execute-bash '(("command"
                                               . "echo 'fail msg'; exit 42")))))
  (check is-err => #t)
  (check-true (string-contains? out "fail msg"))
  (check-true (string-contains? out "Command exited with code 42"))
) ;let-values

(let-values (((out is-err) (pi-execute-bash '(("command" . "sleep 5")
                                              ("timeout" . 1)))))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

(let-values (((out is-err) (pi-execute-bash '(("command" . "echo test")) :timed-out? #t)))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 120 seconds"))
) ;let-values

(check-catch 'error (pi-execute-bash '()))

(check-report)

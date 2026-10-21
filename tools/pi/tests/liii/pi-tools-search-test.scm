(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (liii check) (liii string) (liii path) (liii pi))

(check-set-mode! 'report-failed)

(define test-file
  (if (file-exists? "liii/pi/cli.scm")
    "liii/pi/cli.scm"
    "tools/pi/liii/pi/cli.scm"
  ) ;if
) ;define

;; ============================================================
;; Tool: ls
;; ============================================================

(let ((out (pi-execute-ls '(("path" . ".")))))
  (check-true (string? out))
  (check-true (string-contains? out "tests/"))
) ;let

(let ((out (pi-execute-ls '(("path" . ".") ("limit" . 1)))))
  (check-true (string-contains? out "[1 entries limit reached. Use limit=2 for more]")
  ) ;check-true
) ;let

(let ((empty-dir "tests/liii/pi-empty-dir"))
  (path-mkdir (path empty-dir) :parents #t :exist-ok #t)
  (let ((out (pi-execute-ls (list (cons "path" empty-dir)))))
    (check out => "(empty directory)")
  ) ;let
  (delete-file empty-dir)
) ;let

(check-catch 'error (pi-execute-ls '(("path" . "non_existent_dir_99999"))))
(check-catch 'error (pi-execute-ls (list (cons "path" test-file))))
(check-catch 'error (pi-execute-ls '() :timed-out? #t))

;; ============================================================
;; Tool: find
;; ============================================================

;; 1. 查找 tools/pi 下匹配 *.scm 的文件
(let ((out
        (pi-execute-find
          (list (cons "pattern" "*.scm")
            (cons "path" (if (file-exists? "liii/pi/cli.scm") "." "tools/pi"))
          ) ;list
        ) ;pi-execute-find
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out "cli.scm"))
) ;let

;; 2. limit 生效与提示
(let ((out (pi-execute-find '(("pattern" . "*.scm")
                              ("path" . ".")
                              ("limit" . 1)))))
  (check-true (string-contains? out "[1 results limit reached]"))
) ;let

;; 3. 无匹配文件
(let ((out (pi-execute-find '(("pattern" . "non_existent_pattern_*.xyz")
                              ("path" . ".")))
      ) ;out
     ) ;
  (check out => "No files found matching pattern")
) ;let

;; 4. 路径不存在报错
(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . "non_existent_dir_99999")))
) ;check-catch

;; 5. 超时路径测试
(check-catch 'error (pi-execute-find '(("pattern" . "*.scm")) :timed-out? #t))

;; ============================================================
;; Tool: grep
;; ============================================================

;; 1. 字面量搜索单个文件
(let ((out (pi-execute-grep (list (cons "pattern" "define-library")
                              (cons "path" test-file)
                              (cons "literal" #t)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out ": (define-library (liii pi cli)"))
) ;let

;; 2. 大小写不敏感搜索
(let ((out (pi-execute-grep (list (cons "pattern" "DEFINE-LIBRARY")
                              (cons "path" test-file)
                              (cons "ignoreCase" #t)
                              (cons "literal" #t)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out ": (define-library (liii pi cli)"))
) ;let

;; 3. 正则搜索
(let ((out
        (pi-execute-grep
          (list (cons "pattern" "define.*display")
            (cons "path"
              (if (file-exists? "liii/pi/help.scm")
                "liii/pi/help.scm"
                "tools/pi/liii/pi/help.scm"
              ) ;if
            ) ;cons
          ) ;list
        ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out "display-pi-help"))
) ;let

;; 4. 带 context 搜索
(let ((out (pi-execute-grep (list (cons "pattern" "display-pi-help")
                              (cons "path" test-file)
                              (cons "context" 1)
                              (cons "limit" 1)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out "- "))
  (check-true (string-contains? out ": "))
) ;let

;; 5. limit 生效与提示
(let ((out (pi-execute-grep (list (cons "pattern" "define") (cons "path" test-file) (cons "limit" 1))
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out
                "[1 matches limit reached. Use limit=2 for more, or refine pattern]"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 6. 无匹配结果
(let ((out (pi-execute-grep (list (cons "pattern" "non_existent_symbol_8888") (cons "path" test-file))
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check out => "No matches found")
) ;let

;; 7. 路径不存在报错
(check-catch 'error
  (pi-execute-grep '(("pattern" . "foo") ("path" . "non_existent_file_999")))
) ;check-catch

;; 8. 超时路径测试
(check-catch 'error (pi-execute-grep '(("pattern" . "foo")) :timed-out? #t))

;; 9. 非 git 目录下含子目录的扫描防崩溃测试 (Issue #1153)
(let* ((test-base (path-join (path-temp-dir) "pi-test-grep-1070"))
       (sub-dir (path-join test-base "subdir"))
       (test-f (path-join sub-dir "file.txt"))
      ) ;
  (path-mkdir sub-dir :parents #t :exist-ok #t)
  (path-write-text test-f "target_text_1070\n")
  (let ((out
          (pi-execute-grep (list (cons "pattern" "target_text_1070")
                             (cons "path" (path->string test-base))
                           ) ;list
          ) ;pi-execute-grep
        ) ;out
       ) ;
    (check-true (string? out))
    (check-true (string-contains? out "target_text_1070"))
  ) ;let
  (delete-file (path->string test-f))
  (path-rmdir sub-dir)
  (path-rmdir test-base)
) ;let*

;; 10. 非法正则报错透传 (Issue #1170)
(check-catch 'error
  (pi-execute-grep (list (cons "pattern" "[") (cons "path" test-file)))
) ;check-catch

(check-catch 'error
  (pi-execute-grep
    (list (cons "pattern" "[") (cons "path" (path-parent (path test-file))))
  ) ;pi-execute-grep
) ;check-catch

;; 11. 合法正则无匹配返回 No matches found (Issue #1170)
(let ((out (pi-execute-grep (list (cons "pattern" "non_existent_symbol_[0-9]+") (cons "path" test-file))
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check out => "No matches found")
) ;let

;; 12. 字面量模式下特殊正则符号正常检索 (Issue #1170)
(let ((out (pi-execute-grep (list (cons "pattern" "[") (cons "path" test-file) (cons "literal" #t))
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out "["))
) ;let

(check-report)

;; pi-execute-grep
;; 按正则搜索文件内容
;;
;; 语法
;; ----
;; (pi-execute-grep args)
;;
;; 参数
;; ----
;; args : list?，工具参数
;;
;; 返回值
;; ------
;; record?，工具结果

(import (liii check) (liii string) (liii path) (liii pi code core tools grep))

(check-set-mode! 'report-failed)

(define test-file
  (cond ((file-exists? "tools/pi/tests/fixtures/sample.txt")
         "tools/pi/tests/fixtures/sample.txt"
        ) ;
        ((file-exists? "tests/fixtures/sample.txt") "tests/fixtures/sample.txt")
        ((file-exists? "../../fixtures/sample.txt") "../../fixtures/sample.txt")
        (else "tools/pi/tests/fixtures/sample.txt")
  ) ;cond
) ;define

;; 1. 基本检索
(let ((out (pi-execute-grep `(("pattern" . "sample") ("path" . ,test-file)))))
  (check-true (string? out))
) ;let

;; 2. 合法正则无匹配返回 "No matches found" (Issue #1170)
(let ((out
        (pi-execute-grep `(("pattern" . "non_existent_symbol_[0-9]+")
                           ("path" . ,test-file))
        ) ;pi-execute-grep
      ) ;out
     ) ;
  (check out => "No matches found")
) ;let

;; 3. 非法正则报错透传：文件模式 (Issue #1170)
(check-catch 'error
  (pi-execute-grep `(("pattern" . "[") ("path" . ,test-file)))
) ;check-catch

;; 4. 非法正则报错透传：目录模式 (Issue #1170)
(let* ((tmp-dir (path-join (path-temp-dir) "pi-test-grep-invalid"))
       (tmp-file (path-join tmp-dir "a.txt"))
      ) ;
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-write-text tmp-file "content\n")
  (check-catch 'error
    (pi-execute-grep (list (cons "pattern" "[") (cons "path" (path->string tmp-dir)))
    ) ;pi-execute-grep
  ) ;check-catch
  (delete-file (path->string tmp-file))
  (path-rmdir tmp-dir)
) ;let*

;; 5. 字面量模式按字面量匹配，特殊正则符号不生效 (Issue #1170)
(let* ((tmp-dir (path-join (path-temp-dir) "pi-test-grep-literal"))
       (tmp-file (path-join tmp-dir "bracket.txt"))
      ) ;
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-write-text tmp-file "a [bracket] b\n")
  (let ((out
          (pi-execute-grep (list (cons "pattern" "[bracket]")
                             (cons "path" (path->string tmp-file))
                             (cons "literal" #t)
                           ) ;list
          ) ;pi-execute-grep
        ) ;out
       ) ;
    (check-true (string-contains? out "[bracket]"))
  ) ;let
  (delete-file (path->string tmp-file))
  (path-rmdir tmp-dir)
) ;let*

;; 搜索源码文件：字面量、大小写、正则、context、limit 与错误路径

(define legacy-test-file
  (if (file-exists? "liii/pi/code/cli.scm")
    "liii/pi/code/cli.scm"
    "tools/pi/liii/pi/code/cli.scm"
  ) ;if
) ;define

(let ((out (pi-execute-grep (list (cons "pattern" "define-library")
                              (cons "path" legacy-test-file)
                              (cons "literal" #t)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out ": (define-library (liii pi code cli)"))
) ;let

(let ((out (pi-execute-grep (list (cons "pattern" "DEFINE-LIBRARY")
                              (cons "path" legacy-test-file)
                              (cons "ignoreCase" #t)
                              (cons "literal" #t)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out ": (define-library (liii pi code cli)"))
) ;let

(let ((out
        (pi-execute-grep
          (list (cons "pattern" "define.*display")
            (cons "path"
              (if (file-exists? "liii/pi/code/cli/help.scm")
                "liii/pi/code/cli/help.scm"
                "tools/pi/liii/pi/code/cli/help.scm"
              ) ;if
            ) ;cons
          ) ;list
        ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out "display-pi-help"))
) ;let

(let ((out (pi-execute-grep (list (cons "pattern" "display-pi-help")
                              (cons "path" legacy-test-file)
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

(let ((out (pi-execute-grep (list (cons "pattern" "define") (cons "path" legacy-test-file) (cons "limit" 1))
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string-contains? out
                "[1 matches limit reached. Use limit=2 for more, or refine pattern]"
              ) ;string-contains?
  ) ;check-true
) ;let

(let ((out (pi-execute-grep (list (cons "pattern" "non_existent_symbol_8888")
                              (cons "path" legacy-test-file)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check out => "No matches found")
) ;let

;; 浮点形式的 limit/context：模型偶尔用 1.0 这类浮点表示数量
(let ((out (pi-execute-grep (list (cons "pattern" "define")
                              (cons "path" legacy-test-file)
                              (cons "context" 1.0)
                              (cons "limit" 1.0)
                            ) ;list
           ) ;pi-execute-grep
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out "- "))
  (check-true (string-contains? out
                "[1 matches limit reached. Use limit=2 for more, or refine pattern]"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 非整数值的浮点参数：报清晰的参数错误
(check-catch 'error
  (pi-execute-grep (list (cons "pattern" "define")
                     (cons "path" legacy-test-file)
                     (cons "limit" 1.5)
                   ) ;list
  ) ;pi-execute-grep
) ;check-catch
(check-catch 'error
  (pi-execute-grep (list (cons "pattern" "define")
                     (cons "path" legacy-test-file)
                     (cons "context" 1.5)
                   ) ;list
  ) ;pi-execute-grep
) ;check-catch

(check-catch 'error
  (pi-execute-grep '(("pattern" . "foo") ("path" . "non_existent_file_999")))
) ;check-catch
(check-catch 'error (pi-execute-grep '(("pattern" . "foo")) :timed-out? #t))

;; 非 git 目录下含子目录的扫描防崩溃
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

(check-report)

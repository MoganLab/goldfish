(import (liii check) (liii string) (liii path) (liii pi))

(check-set-mode! 'report-failed)

;; pi-execute-grep
;; 执行 grep 工具按正则表达式或字面量搜索文件内容。

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
(let ((out (pi-execute-grep `(("pattern" . "non_existent_symbol_[0-9]+")
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
    (pi-execute-grep (list (cons "pattern" "[")
                           (cons "path" (path->string tmp-dir))
                         ) ;list
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
  (let ((out (pi-execute-grep (list (cons "pattern" "[bracket]")
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

(check-report)

;; pi-execute-ls
;; 列出目录内容
;;
;; 语法
;; ----
;; (pi-execute-ls args)
;;
;; 参数
;; ----
;; args : list?，工具参数
;;
;; 返回值
;; ------
;; record?，工具结果

(import (liii check) (liii string) (liii path) (liii pi code core tools ls))

(check-set-mode! 'report-failed)

;; pi-execute-ls
;; 执行 ls 工具列出目录文件清单。
(let ((out (pi-execute-ls '(("path" . ".")))))
  (check-true (string? out))
) ;let

(let ((out (pi-execute-ls '(("path" . ".")))))
  (check-true (string-contains? out "tests/"))
) ;let

(let ((out (pi-execute-ls '(("path" . ".") ("limit" . 1)))))
  (check-true (string-contains? out "[1 entries limit reached. Use limit=2 for more]")
  ) ;check-true
) ;let

;; 浮点形式的 limit：模型偶尔用 1.0 这类浮点表示数量
(let ((out (pi-execute-ls '(("path" . ".") ("limit" . 1.0)))))
  (check-true (string-contains? out "[1 entries limit reached. Use limit=2 for more]")
  ) ;check-true
) ;let

;; 非整数值的浮点参数：报清晰的参数错误
(check-catch 'error (pi-execute-ls '(("path" . ".") ("limit" . 1.5))))

(let ((empty-dir "tests/liii/pi-empty-dir"))
  (path-mkdir (path empty-dir) :parents #t :exist-ok #t)
  (let ((out (pi-execute-ls (list (cons "path" empty-dir)))))
    (check out => "(empty directory)")
  ) ;let
  (delete-file empty-dir)
) ;let

(let ((test-file (if (file-exists? "liii/pi/code/cli.scm")
                   "liii/pi/code/cli.scm"
                   "tools/pi/liii/pi/code/cli.scm"
                 ) ;if
      ) ;test-file
     ) ;
  (check-catch 'error (pi-execute-ls (list (cons "path" test-file))))
) ;let
(check-catch 'error (pi-execute-ls '(("path" . "non_existent_dir_99999"))))
(check-catch 'error (pi-execute-ls '() :timed-out? #t))

(check-report)

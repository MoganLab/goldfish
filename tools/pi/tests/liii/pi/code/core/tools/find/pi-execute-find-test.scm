;; pi-execute-find
;; 按名字模式查找文件
;;
;; 语法
;; ----
;; (pi-execute-find args)
;;
;; 参数
;; ----
;; args : list?，工具参数
;;
;; 返回值
;; ------
;; record?，工具结果

(import (liii check) (liii string) (liii pi code core tools find))

(check-set-mode! 'report-failed)

;; pi-execute-find
;; 执行 find 工具按 pattern 查找文件。
(let ((out (pi-execute-find '(("pattern" . "sample.txt")))))
  (check-true (string? out))
) ;let

;; 查找 tools/pi 下匹配 *.scm 的文件
(let ((out
        (pi-execute-find
          (list (cons "pattern" "*.scm")
            (cons "path" (if (file-exists? "liii/pi/code/cli.scm") "." "tools/pi"))
          ) ;list
        ) ;pi-execute-find
      ) ;out
     ) ;
  (check-true (string? out))
  (check-true (string-contains? out "cli.scm"))
) ;let

;; limit 生效与提示
(let ((out (pi-execute-find '(("pattern" . "*.scm")
                              ("path" . ".")
                              ("limit" . 1)))))
  (check-true (string-contains? out "[1 results limit reached]"))
) ;let

;; 浮点形式的 limit：模型偶尔用 1.0 这类浮点表示数量
(let ((out (pi-execute-find '(("pattern" . "*.scm")
                              ("path" . ".")
                              ("limit" . 1.0))))
     ) ;
  (check-true (string-contains? out "[1 results limit reached]"))
) ;let

;; 非整数值的浮点参数：报清晰的参数错误
(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . ".") ("limit" . 1.5)))
) ;check-catch

;; 无匹配文件
(let ((out (pi-execute-find '(("pattern" . "non_existent_pattern_*.xyz")
                              ("path" . ".")))
      ) ;out
     ) ;
  (check out => "No files found matching pattern")
) ;let

(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . "non_existent_dir_99999")))
) ;check-catch
(check-catch 'error (pi-execute-find '(("pattern" . "*.scm")) :timed-out? #t))

(check-report)

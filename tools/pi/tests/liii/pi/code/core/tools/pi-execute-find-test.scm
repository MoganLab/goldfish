(import (liii check) (liii string) (liii pi code core tools find))

(check-set-mode! 'report-failed)

;; pi-execute-find
;; 执行 find 工具按 pattern 查找文件。
(let ((out (pi-execute-find '(("pattern" . "sample.txt")))))
  (check-true (string? out)))

;; 查找 tools/pi 下匹配 *.scm 的文件
(let ((out (pi-execute-find
             (list (cons "pattern" "*.scm")
                   (cons "path" (if (file-exists? "liii/pi/code/cli.scm") "." "tools/pi"))))))
  (check-true (string? out))
  (check-true (string-contains? out "cli.scm")))

;; limit 生效与提示
(let ((out (pi-execute-find '(("pattern" . "*.scm") ("path" . ".") ("limit" . 1)))))
  (check-true (string-contains? out "[1 results limit reached]")))

;; 浮点形式的 limit：模型偶尔用 1.0 这类浮点表示数量
(let ((out (pi-execute-find '(("pattern" . "*.scm") ("path" . ".") ("limit" . 1.0)))))
  (check-true (string-contains? out "[1 results limit reached]")))

;; 非整数值的浮点参数：报清晰的参数错误
(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . ".") ("limit" . 1.5))))

;; 无匹配文件
(let ((out (pi-execute-find '(("pattern" . "non_existent_pattern_*.xyz") ("path" . ".")))))
  (check out => "No files found matching pattern"))

(check-catch 'error
  (pi-execute-find '(("pattern" . "*.scm") ("path" . "non_existent_dir_99999"))))
(check-catch 'error (pi-execute-find '(("pattern" . "*.scm")) :timed-out? #t))

(check-report)

;; rule-cache-path
;; 计算指定规则和参数下目标文件的缓存路径。
;;
;; 语法
;; ----
;; (rule-cache-path rule-name limit file-path)
;;
;; 参数
;; ----
;; rule-name : string  规则名称
;; limit : number      规则参数或版本编号
;; file-path : string  待检测的文件路径
;;
;; 返回值
;; ------
;; string
;; 对应的缓存文件路径字符串。

(import (scheme base) (liii check) (liii string) (liii goldcheck cache))

(check-set-mode! 'report-failed)

(let* ((this-file "tools/check/tests/liii/goldcheck/cache/rule-cache-path-test.scm")
       (alt-file "tests/liii/goldcheck/cache/rule-cache-path-test.scm")
       (target (if (file-exists? this-file) this-file alt-file))
       (path (rule-cache-path "max-file-lines" 400 target))
      ) ;
  (check-true (string? path))
  (check-true (string-contains? path "max-file-lines"))
  (check-true (string-contains? path "400"))
) ;let*

(check-report)

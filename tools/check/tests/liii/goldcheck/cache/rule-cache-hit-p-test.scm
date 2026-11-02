;; rule-cache-hit?
;; 检测指定规则和文件在给定参数下的缓存是否命中。
;;
;; 语法
;; ----
;; (rule-cache-hit? rule-name limit file-path)
;;
;; 参数
;; ----
;; rule-name : string  规则名称
;; limit : number      规则参数或版本编号
;; file-path : string  待检测的文件路径
;;
;; 返回值
;; ------
;; boolean
;; 缓存文件存在返回 #t，否则返回 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii goldcheck cache)
) ;import

(check-set-mode! 'report-failed)

(let* ((this-file "tools/check/tests/liii/goldcheck/cache/rule-cache-hit-p-test.scm")
       (alt-file "tests/liii/goldcheck/cache/rule-cache-hit-p-test.scm")
       (target (if (file-exists? this-file) this-file alt-file))
       (cpath (rule-cache-path "non-existent-rule" 99999 target))
      ) ;
  (when (file-exists? cpath)
    (delete-file cpath)
  ) ;when
  (check (rule-cache-hit? "non-existent-rule" 99999 target) => #f)
) ;let*

(check-report)

;; rule-cache-touch
;; 创建或更新指定规则和文件的缓存标记文件。
;;
;; 语法
;; ----
;; (rule-cache-touch rule-name limit file-path)
;;
;; 参数
;; ----
;; rule-name : string  规则名称
;; limit : number      规则参数或版本编号
;; file-path : string  待检测的文件路径
;;
;; 返回值
;; ------
;; unspecified
;; 创建或更新缓存空文件。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii goldcheck cache)
) ;import

(check-set-mode! 'report-failed)

(let* ((this-file "tools/check/tests/liii/goldcheck/cache/rule-cache-touch-test.scm")
       (alt-file "tests/liii/goldcheck/cache/rule-cache-touch-test.scm")
       (target (if (file-exists? this-file) this-file alt-file))
       (cpath (rule-cache-path "touch-test-rule" 12345 target))
      ) ;
  (when (file-exists? cpath)
    (delete-file cpath)
  ) ;when
  (check (rule-cache-hit? "touch-test-rule" 12345 target) => #f)
  (rule-cache-touch "touch-test-rule" 12345 target)
  (check (rule-cache-hit? "touch-test-rule" 12345 target) => #t)
  (when (file-exists? cpath)
    (delete-file cpath)
  ) ;when
) ;let*

(check-report)

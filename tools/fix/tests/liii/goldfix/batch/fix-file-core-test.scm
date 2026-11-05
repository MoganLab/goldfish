;; fix-file-core
;; 处理单个文件：按修正结果写回磁盘或保持原样，
;; 返回 'cached / 'updated / 'balanced / 'skipped 四种状态之一。
;;
;; 语法
;; ----
;; (fix-file-core path-str repair-source*-fn (use-cache? #t))
;;
;; 参数
;; ----
;; path-str : string? 待处理的文件路径
;; repair-source*-fn : procedure? 返回 (修正后源码 是否跳过) 的修正函数
;; use-cache? : boolean? 是否使用缓存
;;
;; 返回值
;; ------
;; symbol?
;; 'cached 命中缓存，'updated 已写盘，'balanced 本就平衡，'skipped 被守卫拦截。

(import (liii check)
  (liii base)
  (liii path)
  (liii tempfile)
  (liii goldfix)
  (liii goldfix batch)
) ;import

(check-set-mode! 'report-failed)

;; 测试：源码未变化时返回 balanced
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1)")
    (check (fix-file-core file (lambda (src) (values src #f)) #f) => 'balanced)
    (check (path-read-text (path file)) => "(define x 1)")
  ) ;lambda
  "goldfix-core-balanced"
  ".scm"
) ;with-temp-file

;; 测试：源码被改写时返回 updated 并写盘
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1")
    (check
      (fix-file-core file (lambda (src) (values (string-append src ")") #f)) #f)
      =>
      'updated
    ) ;check
    (check (path-read-text (path file)) => "(define x 1)")
  ) ;lambda
  "goldfix-core-updated"
  ".scm"
) ;with-temp-file

;; 测试：修正函数要求跳过时返回 skipped
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1)")
    (check (fix-file-core file (lambda (src) (values src #t)) #f) => 'skipped)
    (check (path-read-text (path file)) => "(define x 1)")
  ) ;lambda
  "goldfix-core-skipped"
  ".scm"
) ;with-temp-file

(check-report)

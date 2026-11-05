;; fix-file
;; 处理单个文件并启用缓存：命中缓存时返回 'cached，
;; 否则行为与 fix-file-core 一致。
;;
;; 语法
;; ----
;; (fix-file path-str repair-source*-fn (use-cache? #t))
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

(import (scheme file)
  (liii check)
  (liii base)
  (liii path)
  (liii tempfile)
  (liii goldfix)
  (liii goldfix batch)
  (liii goldfix cache)
) ;import

(check-set-mode! 'report-failed)

;; 测试：首次处理写盘，再次处理命中缓存
(with-temp-file
  (lambda (file)
    (let ((cache (fix-cache-path file)))
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
      (path-write-text (path file) "(define x 1")
      (check
        (fix-file file (lambda (src) (values (string-append src ")") #f)))
        =>
        'updated
      ) ;check
      (check
        (fix-file file (lambda (src) (values (string-append src ")") #f)))
        =>
        'cached
      ) ;check
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
    ) ;let
  ) ;lambda
  "goldfix-file"
  ".scm"
) ;with-temp-file

;; 测试：关闭缓存时行为与 fix-file-core 一致
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1)")
    (check (fix-file file (lambda (src) (values src #f)) #f) => 'balanced)
  ) ;lambda
  "goldfix-file-nocache"
  ".scm"
) ;with-temp-file

(check-report)

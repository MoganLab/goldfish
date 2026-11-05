;; fix-file-list
;; 批量处理文件列表，返回 (处理总数 已更新数 命中缓存数)。
;; 预览模式只输出预览结果，不修改任何文件。
;;
;; 语法
;; ----
;; (fix-file-list files dry-run repair-source-fn repair-source*-fn)
;;
;; 参数
;; ----
;; files : list? 待处理的文件路径列表
;; dry-run : boolean? 是否为预览模式
;; repair-source-fn : procedure? 源码修正函数（预览模式使用）
;; repair-source*-fn : procedure? 返回 (修正后源码 是否跳过) 的修正函数
;;
;; 返回值
;; ------
;; values
;; 第一个值为处理的文件总数，第二个值为已更新的文件数，第三个值为命中缓存的文件数。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii base)
  (liii path)
  (liii tempfile)
  (liii goldfix)
  (liii goldfix batch)
  (liii goldfix cache)
) ;import

(check-set-mode! 'report-failed)

;; 测试：预览模式只输出预览结果，不修改文件
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1")
    (let ((result '()))
      (with-output-to-string
        (lambda ()
          (call-with-values
            (lambda ()
              (fix-file-list (list file)
                #t
                (lambda (src) (string-append src ")"))
                (lambda (src) (values src #f))
              ) ;fix-file-list
            ) ;lambda
            (lambda (total updated cached) (set! result (list total updated cached)))
          ) ;call-with-values
        ) ;lambda
      ) ;with-output-to-string
      (check result => '(1 0 0))
      (check (path-read-text (path file)) => "(define x 1")
    ) ;let
  ) ;lambda
  "goldfix-file-list-dry"
  ".scm"
) ;with-temp-file

;; 测试：非预览模式写回文件并统计已更新数量
(with-temp-file
  (lambda (file)
    (let ((cache (fix-cache-path file)))
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
      (path-write-text (path file) "(define x 1")
      (let ((result '()))
        (with-output-to-string
          (lambda ()
            (call-with-values
              (lambda ()
                (fix-file-list (list file)
                  #f
                  (lambda (src) src)
                  (lambda (src) (values (string-append src ")") #f))
                ) ;fix-file-list
              ) ;lambda
              (lambda (total updated cached) (set! result (list total updated cached)))
            ) ;call-with-values
          ) ;lambda
        ) ;with-output-to-string
        (check result => '(1 1 0))
        (check (path-read-text (path file)) => "(define x 1)")
      ) ;let
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
    ) ;let
  ) ;lambda
  "goldfix-file-list"
  ".scm"
) ;with-temp-file

(check-report)

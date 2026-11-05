;; fix-directory
;; 递归处理目录下匹配后缀名的文件，返回 (处理总数 已更新数 命中缓存数)。
;;
;; 语法
;; ----
;; (fix-directory dir-path extensions repair-source*-fn)
;;
;; 参数
;; ----
;; dir-path : string? 待处理的目录路径
;; extensions : list? 参与处理的文件后缀名列表
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

;; 测试：只处理后缀名匹配的文件
(with-temp-dir
  (lambda (dir)
    (let* ((scm-file (path->string (path-join (path dir) "a.scm")))
           (txt-file (path->string (path-join (path dir) "b.txt")))
          ) ;
      (path-write-text (path scm-file) "(define a 1")
      (path-write-text (path txt-file) "text")
      (let ((cache (fix-cache-path scm-file)))
        (when (file-exists? cache)
          (delete-file cache)
        ) ;when
        (let ((result '()))
          (with-output-to-string
            (lambda ()
              (call-with-values
                (lambda ()
                  (fix-directory dir '(".scm") (lambda (src) (values (string-append src ")") #f)))
                ) ;lambda
                (lambda (total updated cached) (set! result (list total updated cached)))
              ) ;call-with-values
            ) ;lambda
          ) ;with-output-to-string
          (check result => '(1 1 0))
          (check (path-read-text (path scm-file)) => "(define a 1)")
          (check (path-read-text (path txt-file)) => "text")
        ) ;let
        (when (file-exists? cache)
          (delete-file cache)
        ) ;when
      ) ;let
    ) ;let*
  ) ;lambda
  "goldfix-directory"
) ;with-temp-dir

(check-report)

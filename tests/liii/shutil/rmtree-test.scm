(import (liii check)
        (liii error)
        (liii os)
        (liii string)
        (liii shutil))


(check-set-mode! 'report-failed)


;; rmtree
;; 递归删除目录树（目录及其全部内容）。
;;
;; 语法
;; ----
;; (rmtree path)
;; (rmtree path ignore-errors)
;;
;; 参数
;; ----
;; path : string?
;; 要删除的目录路径。
;; ignore-errors : boolean?
;; 为 #t 时忽略错误（删除失败返回 #f 而不抛错），
;; 默认为 #f。
;;
;; 返回值
;; -----
;; boolean?
;; 删除成功返回 #t。
;; ignore-errors 为 #t 且删除失败时返回 #f。
;;
;; 错误
;; ----
;; type-error
;; 当 path 不是字符串时抛出错误。
;;
;; file-not-found-error
;; 当 path 不存在时抛出错误（ignore-errors 为 #t 时不抛出）。
;;
;; os-error
;; 当删除失败时抛出错误（ignore-errors 为 #t 时不抛出）。
;;
;; 测试安全约定
;; ------------
;; rmtree 是破坏性操作，所有测试只允许删除系统临时目录
;; (os-temp-dir) 下面的子目录，绝不允许触碰其他路径。


;; ; 安全护栏：path 必须是临时目录下严格更深层的子路径，
;; ; 否则立即报错中止，不执行任何删除。
(define (assert-temp-subdir! path)
  (let ((prefix (string-append (os-temp-dir) (string (os-sep)))))
    (unless (and (string-starts? path prefix)
                 (> (string-length path) (string-length prefix)))
      (value-error "rmtree 测试只允许删除临时目录下的子目录:" path))
  ) ;let
) ;define

;; ; 预清理：清理上次运行可能残留的测试目录
(define (cleanup-temp-dir path)
  (when (file-exists? path)
    (assert-temp-subdir! path)
    (rmtree path #t)
  ) ;when
) ;define


;; ; 基本功能测试：递归删除多层目录树
(let* ((temp-dir (os-temp-dir))
       (test-dir (string-append temp-dir (string (os-sep)) "test_rmtree_dir"))
       (sub-dir (string-append test-dir (string (os-sep)) "sub"))
       (sub-sub-dir (string-append sub-dir (string (os-sep)) "deep"))
      )
  (assert-temp-subdir! test-dir)
  (cleanup-temp-dir test-dir)

  ;; 构造目录树: test-dir/{a.txt, sub/{b.txt, deep/c.txt}}
  (mkdir test-dir)
  (mkdir sub-dir)
  (mkdir sub-sub-dir)
  (with-output-to-file (string-append test-dir (string (os-sep)) "a.txt")
    (lambda () (display "aaa")))
  (with-output-to-file (string-append sub-dir (string (os-sep)) "b.txt")
    (lambda () (display "bbb")))
  (with-output-to-file (string-append sub-sub-dir (string (os-sep)) "c.txt")
    (lambda () (display "ccc")))
  (check-true (file-exists? test-dir))
  (check-true (file-exists? (string-append sub-sub-dir (string (os-sep)) "c.txt")))

  ;; 递归删除
  (check-true (rmtree test-dir))
  (check-false (file-exists? test-dir))
) ;let*


;; ; 删除空目录
(let* ((temp-dir (os-temp-dir))
       (test-dir (string-append temp-dir (string (os-sep)) "test_rmtree_empty")))
  (assert-temp-subdir! test-dir)
  (cleanup-temp-dir test-dir)
  (mkdir test-dir)
  (check-true (rmtree test-dir))
  (check-false (file-exists? test-dir))
) ;let*


;; ; ignore-errors 为 #t 时删除成功同样返回 #t
(let* ((temp-dir (os-temp-dir))
       (test-dir (string-append temp-dir (string (os-sep)) "test_rmtree_flag")))
  (assert-temp-subdir! test-dir)
  (cleanup-temp-dir test-dir)
  (mkdir test-dir)
  (check-true (rmtree test-dir #t))
  (check-false (file-exists? test-dir))
) ;let*


;; ; 路径不存在：默认抛 file-not-found-error
;; ; 路径本身位于临时目录下且不存在，不会删除任何真实数据
(check-catch 'file-not-found-error
  (rmtree (string-append (os-temp-dir) (string (os-sep)) "no-such-dir-xyz-1126")))

;; ; 路径不存在：ignore-errors 为 #t 时返回 #f
(check-false
  (rmtree (string-append (os-temp-dir) (string (os-sep)) "no-such-dir-xyz-1126") #t))


;; ; 错误测试
(check-catch 'type-error (rmtree #\a))
(check-catch 'type-error (rmtree 123))


(check-report)

(import (liii check)
  (liii path)
  (liii os)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-rglob
;; 全目录树递归通配检索，返回包含匹配项的 <path> 记录列表。
;;
;; 语法
;; ----
;; (path-rglob path pattern) -> list
;;
;; 参数
;; ----
;; path : string | path?
;; 根目录路径。
;; pattern : string
;; 通配符模式，支持 *（任意多个字符）、?（任意单个字符）、[seq]（字符集）。
;;
;; 返回值
;; -----
;; list of path
;; 包含全目录树中所有匹配项对应的 <path> 记录对象列表；无匹配时返回 '()。
;;
;; 错误处理
;; ----
;; type-error 当 path 非 string 且非 path?，或 pattern 非 string 时。
;; file-not-found-error 当 path 路径不存在时。
;; not-a-directory-error 当 path 存在但不是目录时。

(define (clean-dir dir)
  (when (path-exists? dir)
    (for-each
      (lambda (item)
        (if (path-dir? item)
          (clean-dir item)
          (delete-file (path->string item))
        ) ;if
      ) ;lambda
      (path-list-path dir)
    ) ;for-each
    (path-rmdir dir)
  ) ;when
) ;define

(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)
  ) ;cond
) ;define

(let* ((test-dir (path-join (path-temp-dir) "gf-test-path-rglob"))
       (sub1 (path-join test-dir "sub1"))
       (sub2 (path-join sub1 "sub2"))
       (f-test1 (path-join sub1 "test1.scm"))
       (f-test2 (path-join sub2 "test2.scm"))
       (f-txt (path-join sub1 "test.txt"))
       (f-root (path-join test-dir "root.scm"))
      ) ;
  (clean-dir test-dir)
  (path-mkdir sub2 #t #t)
  (path-write-text f-test1 "test1\n")
  (path-write-text f-test2 "test2\n")
  (path-write-text f-txt "text\n")
  (path-write-text f-root "root\n")

  ;; 测试项2：全目录树递归通配检索 path-rglob
  (let ((res (path-rglob test-dir "*.scm")))
    (check (length res) => 3)
    (check-true (every path? res))
    (check (map path-name res) => '("root.scm" "test2.scm" "test1.scm"))
    ;; 验证元素天然支持 path 接口
    (check-true (> (path-getsize (car res)) 0))
    (check-true (> (path-getsize (cadr res)) 0))
    (check-true (> (path-getsize (caddr res)) 0))
  ) ;let

  ;; 接受字符串参数
  (let ((res (path-rglob (path->string test-dir) "*.scm")))
    (check (length res) => 3)
    (check (map path-name res) => '("root.scm" "test2.scm" "test1.scm"))
  ) ;let

  ;; 无任何匹配项模式返回 '()
  (check (path-rglob test-dir "*.xyz") => '())

  ;; 测试项3：通配符模式丰富性（? 与 [abc] 及多段模式）
  ;; 单个字符通配符 ?
  (let ((res-q (path-rglob test-dir "test?.scm")))
    (check (length res-q) => 2)
    (check (map path-name res-q) => '("test2.scm" "test1.scm"))
  ) ;let

  ;; 多段尾部匹配
  (let ((res-sub2 (path-rglob test-dir "sub2/*.scm")))
    (check (length res-sub2) => 1)
    (check (map path-name res-sub2) => '("test2.scm"))
  ) ;let

  ;; 字符集匹配
  (let ((res-class (path-rglob test-dir "test[13].scm")))
    (check (length res-class) => 1)
    (check (map path-name res-class) => '("test1.scm"))
  ) ;let

  ;; 字符集取反
  (let ((res-neg (path-rglob test-dir "test[^1].scm")))
    (check (length res-neg) => 1)
    (check (map path-name res-neg) => '("test2.scm"))
  ) ;let

  ;; 测试项4：路径有效性与防御性异常测试
  (check-catch 'file-not-found-error
    (path-rglob "no-such-path-rglob-1053-xyz" "*")
  ) ;check-catch
  (check-catch 'file-not-found-error
    (path-rglob (path "no-such-path-rglob-1053-xyz") "*")
  ) ;check-catch
  (check-catch 'not-a-directory-error
    (path-rglob f-txt "*")
  ) ;check-catch
  (check-catch 'not-a-directory-error
    (path-rglob (path->string f-txt) "*")
  ) ;check-catch
  (check-catch 'type-error (path-rglob 123 "*"))
  (check-catch 'type-error (path-rglob test-dir 456))
  (check-catch 'type-error (path-rglob '(invalid) "*"))
  (check-catch 'type-error (path-rglob test-dir 'pattern))

  ;; 清理测试目录
  (clean-dir test-dir)
) ;let*

(check-report)

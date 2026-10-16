(import (liii check) (liii path) (liii os) (scheme file))

(check-set-mode! 'report-failed)

;; path-glob
;; 单层目录通配检索，返回包含匹配项的 <path> 记录列表。
;;
;; 语法
;; ----
;; (path-glob path pattern) -> list
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
;; 包含所有匹配项对应的 <path> 记录对象列表；无匹配时返回 '()。
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
        (if (path-dir? item) (clean-dir item) (delete-file (path->string item)))
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

(let* ((test-dir (path-join (path-temp-dir) "gf-test-path-glob"))
       (file-a (path-join test-dir "a.scm"))
       (file-b (path-join test-dir "b.scm"))
       (file-c (path-join test-dir "c.txt"))
       (sub-dir (path-join test-dir "sub"))
      ) ;
  (clean-dir test-dir)
  (path-mkdir test-dir #t #t)
  (path-write-text file-a "(define a 1)\n")
  (path-write-text file-b "(define b 2)\n")
  (path-write-text file-c "text file\n")
  (path-mkdir sub-dir #f #t)

  ;; 测试项1：单层目录通配检索 path-glob
  (let ((res (path-glob test-dir "*.scm")))
    (check (length res) => 2)
    (check-true (every path? res))
    (check (map path-name res) => '("a.scm" "b.scm"))
    ;; 验证返回的 path 对象天然支持 path 接口
    (check-true (> (path-getsize (car res)) 0))
    (check-true (> (path-getsize (cadr res)) 0))
  ) ;let

  ;; 验证接受字符串路径参数
  (let ((res (path-glob (path->string test-dir) "*.scm")))
    (check (length res) => 2)
    (check (map path-name res) => '("a.scm" "b.scm"))
  ) ;let

  ;; 验证通配符 * 匹配所有直接子项（包含子目录与非 .scm 文件）
  (let ((all-res (path-glob test-dir "*")))
    (check (length all-res) => 4)
    (check (map path-name all-res) => '("a.scm" "b.scm" "c.txt" "sub"))
  ) ;let

  ;; 验证无匹配项返回 '()
  (check (path-glob test-dir "*.xyz") => '())

  ;; 测试项3：通配符模式丰富性（? 与 [abc]）
  (let ((res-q (path-glob test-dir "?.scm")))
    (check (length res-q) => 2)
    (check (map path-name res-q) => '("a.scm" "b.scm"))
  ) ;let
  (let ((res-class (path-glob test-dir "[ab].scm")))
    (check (length res-class) => 2)
    (check (map path-name res-class) => '("a.scm" "b.scm"))
  ) ;let
  (let ((res-class2 (path-glob test-dir "[ac].scm")))
    (check (length res-class2) => 1)
    (check (map path-name res-class2) => '("a.scm"))
  ) ;let
  (let ((res-range (path-glob test-dir "[a-z].scm")))
    (check (length res-range) => 2)
    (check (map path-name res-range) => '("a.scm" "b.scm"))
  ) ;let
  (let ((res-neg (path-glob test-dir "[^b].scm")))
    (check (length res-neg) => 1)
    (check (map path-name res-neg) => '("a.scm"))
  ) ;let

  ;; 测试项4：路径有效性与防御性异常测试
  (check-catch 'file-not-found-error (path-glob "no-such-path-glob-1053-xyz" "*"))
  (check-catch 'file-not-found-error
    (path-glob (path "no-such-path-glob-1053-xyz") "*")
  ) ;check-catch
  (check-catch 'not-a-directory-error (path-glob file-c "*"))
  (check-catch 'not-a-directory-error (path-glob (path->string file-c) "*"))
  (check-catch 'type-error (path-glob 123 "*"))
  (check-catch 'type-error (path-glob test-dir 456))
  (check-catch 'type-error (path-glob '(invalid) "*"))
  (check-catch 'type-error (path-glob test-dir 'pattern))

  ;; 清理测试目录
  (clean-dir test-dir)
) ;let*

(check-report)

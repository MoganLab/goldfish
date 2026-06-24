;; goldfish (liii path) POSIX 实际行为审计探针
;;
;; 用途:与 pathlib-ref/posix_ref.py 的 PurePosixPath 真值逐行对照。
;; label 与 posix_ref.py 一一对应(顺序、用例相同),便于 diff。
;;
;; 运行: bin/gf pathlib-ref/posix_audit.scm
;; 对照: diff <(python3 pathlib-ref/posix_ref.py) <(bin/gf pathlib-ref/posix_audit.scm)
;;
;; 注意 value 格式差异:python repr 给 '...' / [..] / True,False;
;; scheme write 给 "..." / (..) / #t,#f。label 对齐即可定位差异。

(import (liii path) (liii os) (liii base) (liii string))

(define (show label v)
  (display label) (display " => ") (write v) (newline))

(define (list->show label lst)
  ;; 与 python list 对应:scheme 用 (...)
  (display label) (display " => ") (write lst) (newline))

;; ---- str (path->string) ----
(for-each
  (lambda (s) (show (string-append "str " s) (path->string (path s))))
  '("." "" ".." "a/b" "a//b" "a/./b" "./a" "a/" "/a/" "a/../b" "/.." "/." "/" "/a/b/c" "tmp/demo.txt" "a.b.c" ".hidden" "foo.")
) ;for-each

;; ---- name ----
(for-each
  (lambda (s) (show (string-append "name " s) (path-name (path s))))
  '("." ".." "" "a/" "/a/b/" "a..b" "foo." "a...b" "file.txt" ".hidden")
) ;for-each

;; ---- stem / suffix / suffixes ----
(for-each
  (lambda (s)
    (show (string-append "stem " s) (path-stem (path s)))
    (show (string-append "suffix " s) (path-suffix (path s)))
    (list->show (string-append "suffixes " s) (vector->list (path-suffixes (path s))))
  ) ;lambda
  '("file.txt" "archive.tar.gz" ".hidden" "noext" "" "." ".." "foo." "a..b" "a...b" "a.b.c" "config.yaml.bak")
) ;for-each

;; ---- parent ----
(for-each
  (lambda (s) (show (string-append "parent " s) (path->string (path-parent (path s)))))
  '("." ".." "" "/" "/a" "/a/b" "/a/b/c" "a" "a/b" "a/" "/a/" "a/../b")
) ;for-each

;; ---- parents ----
(for-each
  (lambda (s)
    (list->show (string-append "parents " s)
      (map path->string (vector->list (path-parents (path s))))))
  '("/a/b/c" "/a/b" "/a" "/" "a/b/c" "a" "." "..")
) ;for-each

;; ---- parts ----
(for-each
  (lambda (s) (list->show (string-append "parts " s) (vector->list (path-parts (path s)))))
  '("." "" ".." "/" "/a/b" "a/b" "/a/" "a/" "/.." "a//b")
) ;for-each

;; ---- joinpath ----
;; 用例与 posix_ref.py 的 joins 表一致,label 为 "join {base} {segs空格连接}"
(for-each
  (lambda (c)
    (let* ((base (car c)) (segs (cadr c))
           (joined-segs (apply string-append (map (lambda (x) (string-append " " x)) segs))))
      (show (string-append "join " base joined-segs)
        (path->string (apply path-join (cons (path base) (map path segs))))))
  ) ;lambda
  '(("/a" ("/b")) ("/a" ("b")) ("a" ("/b")) ("/a" ("b" "/c" "d"))
    ("/a" ("")) ("a" ("")) ("" ("b")) ("" ("")) ("a" ())
    ("/" ("tmp" "demo.txt")) ("/a/" ("b")))
) ;for-each

;; ---- match ----
(for-each
  (lambda (c)
    (show (string-append "match " (car c) " " (cadr c))
      (path-match (path (car c)) (cadr c))))
  '(("a/b/c" "b/c") ("a/b/c" "c") ("a/b/c" "b") ("a/b/c" "a/b/c")
    ("a/b/c" "*/c") ("/a/b/c" "/a/b/c") ("/a/b/c" "a/b/c")
    ("/x/a/b/c" "/a/b/c") ("/a/b" "a/b") ("a/b/c" "a/c")
    ("foo.txt" "*.txt") ("foo.TXT" "*.txt") ("foo.txt" "foo.???"))
) ;for-each

;; ---- is_absolute ----
(for-each
  (lambda (s) (show (string-append "abs? " s) (path-absolute? (path s))))
  '("/a" "a" "." "/" "/a/b" "a/b")
) ;for-each

;; ---- relative_to ----
(for-each
  (lambda (c)
    (show (string-append "rel-to " (car c) " " (cadr c))
      (path->string (path-relative-to (path (car c)) (path (cadr c))))))
  '(("/a/b" "/a") ("/a" "/a") ("/a/b/c" "/a") ("/a/b" "/") ("a/b/c" "a/b") ("a/b" "a/b"))
) ;for-each

;; ---- with_name / with_stem / with_suffix ----
(for-each
  (lambda (c)
    (show (string-append "with-name " (car c) " " (cadr c))
      (path->string (path-with-name (path (car c)) (cadr c)))))
  '(("a.txt" "c.md") ("/a/b.txt" "c.md") ("x.txt" "y"))
) ;for-each
(for-each
  (lambda (c)
    (show (string-append "with-stem " (car c) " " (cadr c))
      (path->string (path-with-stem (path (car c)) (cadr c)))))
  '(("a.tar.gz" "new") ("a.b.c" "new") ("a.txt" "b") ("README" "new") (".bashrc" "new") ("foo." "new"))
) ;for-each
(for-each
  (lambda (c)
    (show (string-append "with-suffix " (car c) " " (cadr c))
      (path->string (path-with-suffix (path (car c)) (cadr c)))))
  '(("a.txt" ".md") ("README" ".md") ("a.tar.gz" ".md") ("a.txt" "") ("a.tar.gz" "") (".bashrc" ".bak"))
) ;for-each

;; ---- as_posix ----
(for-each
  (lambda (s) (show (string-append "as-posix " s) (path-as-posix (path s))))
  '("/a/b/c" "a/b" "/" ".")
) ;for-each

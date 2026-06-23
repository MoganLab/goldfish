;; goldfish (liii path) Windows 实际行为审计探针
;;
;; 用途:与 pathlib-ref/windows_ref.py 的 PureWindowsPath 真值逐行对照。
;; label 与 windows_ref.py 一一对应(顺序、用例相同),便于 diff。
;;
;; 运行(Windows 平台): bin/gf pathlib-ref/windows_audit.scm
;; 对照: diff <(python3 pathlib-ref/windows_ref.py) <(bin/gf pathlib-ref/windows_audit.scm)
;;       或: python3 pathlib-ref/normalize.py windows
;;
;; 重要:本文件须在 Windows 平台运行。macOS 上 (path "C:\\a") 不解析为 windows
;; (走 PurePosixPath 语义,整体当一段),只有 (os-windows?)=>#t 时字符串才解析为
;; windows record。故 macOS 上跑此文件结果不代表 Windows 行为。
;;
;; 注意 value 格式差异:python repr 给 '...' / [..] / True,False;
;; scheme write 给 "..." / (..) / #t,#f。label 对齐即可定位差异。

(import (liii path) (liii os) (liii base) (liii string))

(define (show label v)
  (display label) (display " => ") (write v) (newline))

(define (list->show label lst)
  (display label) (display " => ") (write lst) (newline))

;; ---- str (path->string) ----
(for-each
  (lambda (s) (show (string-append "str " s) (path->string (path s))))
  '("C:\\a" "C:\\a\\b" "C:\\foo" "C:foo" "C:foo\\bar" "C:\\"
    "\\foo" "\\" "\\\\srv" "\\\\srv\\sh" "\\\\srv\\sh\\a" "\\\\srv\\sh\\a\\b"
    "\\\\srv\\share\\a\\b" "\\\\srv\\share" "foo\\bar" "C:/Users/foo" "C:/a/b")
) ;for-each

;; ---- drive ----
(for-each
  (lambda (s) (show (string-append "drive " s) (path-drive (path s))))
  '("C:\\a" "C:\\a\\b" "C:\\foo" "C:foo" "C:foo\\bar" "C:\\"
    "\\foo" "\\" "\\\\srv" "\\\\srv\\sh" "\\\\srv\\sh\\a" "\\\\srv\\sh\\a\\b"
    "\\\\srv\\share\\a\\b" "\\\\srv\\share" "foo\\bar" "C:/Users/foo" "C:/a/b")
) ;for-each

;; ---- root ----
;; goldfish 暂未导出 root 访问器;此处用 path->string 间接体现。
;; 精确 root 对照待实现导出 root getter 后补。
(for-each
  (lambda (s) (show (string-append "root " s) (path->string (path s))))
  '("C:\\a" "C:foo" "\\foo" "\\\\srv\\sh\\a")
) ;for-each

;; ---- name / stem / suffix / suffixes ----
(for-each
  (lambda (s)
    (show (string-append "name " s) (path-name (path s)))
    (show (string-append "stem " s) (path-stem (path s)))
    (show (string-append "suffix " s) (path-suffix (path s)))
    (list->show (string-append "suffixes " s) (vector->list (path-suffixes (path s)))))
  '("C:\\tmp\\demo.txt" "C:\\a\\b.tar.gz" "C:\\Users\\foo\\bar.tar.gz" "\\\\srv\\sh\\a.txt")
) ;for-each

;; ---- parent ----
(for-each
  (lambda (s) (show (string-append "parent " s) (path->string (path-parent (path s)))))
  '("C:\\Users" "\\foo" "C:foo\\bar" "C:foo" "C:\\"
    "\\\\srv\\sh\\a" "\\\\srv\\sh\\a\\b" "\\\\srv\\sh" "C:\\a\\b\\c")
) ;for-each

;; ---- parents ----
(for-each
  (lambda (s)
    (list->show (string-append "parents " s)
      (map path->string (vector->list (path-parents (path s))))))
  '("C:\\a\\b" "C:\\a" "C:\\" "C:foo\\bar" "\\\\srv\\sh\\a\\b"
    "\\\\srv\\sh\\a" "\\\\srv\\sh" "\\foo")
) ;for-each

;; ---- parts ----
(for-each
  (lambda (s) (list->show (string-append "parts " s) (vector->list (path-parts (path s)))))
  '("C:\\tmp\\demo.txt" "\\\\srv\\sh\\a\\b" "C:foo" "\\foo" "C:\\" "\\\\srv\\sh")
) ;for-each

;; ---- is_absolute ----
(for-each
  (lambda (s) (show (string-append "abs? " s) (path-absolute? (path s))))
  '("\\foo" "C:\\foo" "C:foo" "\\\\srv\\sh\\foo" "\\" "C:\\" "\\\\srv\\sh")
) ;for-each

;; ---- joinpath ----
;; label "join {base} {segs空格连接}",与 windows_ref.py 一致。
(for-each
  (lambda (c)
    (let* ((base (car c)) (segs (cadr c))
           (joined-segs (apply string-append (map (lambda (x) (string-append " " x)) segs))))
      (show (string-append "join " base joined-segs)
        (path->string (apply path-join (cons (path base) (map path segs))))))
  ) ;lambda
  '(("C:\\a" ("\\b"))                  ; => C:\b (drive 继承)
    ("C:\\base" ("D:rel" "x.txt"))     ; => D:rel\x.txt (不同 drive 重置)
    ("C:\\base" ("C:rel"))             ; => C:\base\rel (同 drive 合并)
    ("C:\\a" ("D:\\b"))                ; => D:\b (完整绝对重置)
    ("\\\\srv\\sh" ("\\a"))            ; => \\srv\sh\a
    ("C:\\Users" ("foo" "bar.scm")))   ; => C:\Users\foo\bar.scm
) ;for-each

;; ---- relative_to ----
(for-each
  (lambda (c)
    (show (string-append "rel-to " (car c) " " (cadr c))
      (path->string (path-relative-to (path (car c)) (path (cadr c))))))
  '(("C:\\a\\b\\c" "C:\\a") ("C:\\a\\b" "C:\\a\\b") ("C:\\a\\b" "C:\\") ("\\\\srv\\sh\\a\\b" "\\\\srv\\sh"))
) ;for-each

;; ---- with_name / with_stem / with_suffix ----
(for-each
  (lambda (c)
    (show (string-append "with-name " (car c) " " (cadr c))
      (path->string (path-with-name (path (car c)) (cadr c)))))
  '(("C:\\a\\b.txt" "c.md") ("\\\\srv\\sh\\a.txt" "b.md"))
) ;for-each
(for-each
  (lambda (c)
    (show (string-append "with-stem " (car c) " " (cadr c))
      (path->string (path-with-stem (path (car c)) (cadr c)))))
  '(("C:\\tmp\\a.tar.gz" "new"))
) ;for-each
(for-each
  (lambda (c)
    (show (string-append "with-suffix " (car c) " " (cadr c))
      (path->string (path-with-suffix (path (car c)) (cadr c)))))
  '(("C:\\tmp\\a.txt" ".md") ("C:\\tmp\\a.tar.gz" "") ("\\\\srv\\sh\\a.txt" ".md"))
) ;for-each

;; ---- as_posix ----
(for-each
  (lambda (s) (show (string-append "as-posix " s) (path-as-posix (path s))))
  '("C:\\a\\b" "\\\\srv\\sh\\a" "C:foo\\bar" "C:\\")
) ;for-each

;; ---- match (windows, 大小写不敏感) ----
(for-each
  (lambda (c)
    (show (string-append "match " (car c) " " (cadr c))
      (path-match (path (car c)) (cadr c))))
  '(("Foo.TXT" "*.txt") ("C:\\a\\b\\foo.txt" "*.txt"))
) ;for-each

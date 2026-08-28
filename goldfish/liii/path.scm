(define-library (liii path)
  (import (goldfish))
  (export path
    path?
    path-copy
    path-copy-into
    path-dir?
    path-file?
    path-exists?
    path-getsize
    path-read-text
    path-read-bytes
    path-write-text
    path-write-bytes
    path-append-text
    path-touch
    path-root
    path-of-drive
    path-from-parts
    path-from-env
    path-cwd
    path-home
    path-temp-dir
    path-parts
    path-type
    path-drive
    path->string
    path-from-string
    path-name
    path-stem
    path-suffix
    path-suffixes
    path-with-name
    path-with-stem
    path-with-suffix
    path-relative-to
    path-starts-with?
    path-equals?
    path=?
    path-absolute?
    path-relative?
    path-join
    path-parent
    path-parents
    path-list
    path-list-path
    path-rmdir
    path-unlink
    path-rename
    path-mkdir
    path-absolute
    path-expanduser
    path-match
    path-as-posix
    path-resolve
  ) ;export
  (import (liii base)
    (liii error)
    (liii os)
    (liii string)
    (liii vector)
    (scheme base)
    (scheme char)
    (liii ascii)
  ) ;import
  (begin

    ;; ;============================================================
    ;; ; Path record 类型
    ;; ;============================================================
    ;; ; root 字段：#\\ / #\/ 表示有根分隔符(绝对路径或驱动器根),
    ;; ; #f 表示无根(drive-relative 或相对路径)。用于区分 C:\foo
    ;; ; (root=#\\)和 C:foo(root=#f)的语义差异。
    (define-record-type <path>
      (make-path-record parts type drive root)
      path?
      (parts path-record-parts path-record-set-parts!)
      (type path-record-type path-record-set-type!)
      (drive path-record-drive path-record-set-drive!)
      (root path-record-root path-record-set-root!)
    ) ;define-record-type

    ;; ;============================================================
    ;; ; 辅助函数区
    ;; ;============================================================

    (include "liii/path/parse.scm")

    (include "liii/path/pure.scm")

    ;; ; 返回末段替换为 new-name 后的新 path(保留 drive/root/其他段)。
    ;; ; 差异:替换后 Windows drive/anchor 保留(如 C:\a\b.txt → C:\a\c.md),
    ;; ; posix 同理(/a/b.txt → /a/c.md)。
    ;; ; 空路径(parts 为空或仅 ".")直接返回单段 new-name 的相对路径。
    (define (replace-last-segment p new-name)
      (let* ((pp (path p))
             (parts (path-record-parts pp))
             (n (vector-length parts))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
             (root (path-record-root pp))
            ) ;
        (cond ((or (= n 0) (and (= n 1) (string=? (vector-ref parts 0) ".")))
               (make-path-record (vector new-name) 'posix "" #f)
              ) ;
              (else (let ((new-parts (vector-copy parts)))
                      (vector-set! new-parts (- n 1) new-name)
                      (make-path-record new-parts type drive root)
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define



    ;; ; 规范化绝对路径:消除 . 段、折叠 .. 段。
    ;; ; 不解析符号链接(无 realpath 原语,与 pathlib strict 语义有差异)。
    (define (normalize-absolute p)
      (let* ((pp (path-absolute p))
             (segs (vector->list (path-record-parts pp)))
             (root (path-record-root pp))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
            ) ;
        (let loop
          ((rest segs) (acc '()))
          (cond ((null? rest) (make-path-record (list->vector (reverse acc)) type drive root))
                ((string=? (car rest) "..")
                 (if (null? acc)
                   ;; .. 在根之上:丢弃(不能越过根)
                   (loop (cdr rest) acc)
                   (loop (cdr rest) (cdr acc))
                 ) ;if
                ) ;
                (else (loop (cdr rest) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (include "liii/path/ops.scm")

  ) ;begin
) ;define-library

;; split-name-dots
;; 解析文件名末段的点分隔结构。
;;
;; 语法
;; ----
;; (split-name-dots name) -> values
;;
;; 参数
;; ----
;; name : string?
;; 文件末段的名字。
;;
;; 返回值
;; ----
;; values
;; 两个值：stem 段列表与后缀段列表（含点前缀）。
;;
;; 描述
;; ----
;; 隐藏文件（.bashrc）、无点名字、"." 与 ".." 整体作为 stem 且无后缀；
;; 末尾点（foo.）当作空后缀：stem 为 "foo"，后缀为 "."。

(import (liii check) (srfi srfi-11) (liii path component))

(check-set-mode! 'report-failed)

(let-values (((stem-segs suffix-segs) (split-name-dots "file.txt")))
  (check stem-segs => '("file"))
  (check suffix-segs => '(".txt"))
) ;let-values

;; 多后缀：只拆出最后一个后缀
(let-values (((stem-segs suffix-segs) (split-name-dots "archive.tar.gz")))
  (check stem-segs => '("archive" "tar"))
  (check suffix-segs => '(".gz"))
) ;let-values

;; 无点名字整体作 stem
(let-values (((stem-segs suffix-segs) (split-name-dots "noext")))
  (check stem-segs => '("noext"))
  (check suffix-segs => '())
) ;let-values

;; 隐藏文件整体作 stem
(let-values (((stem-segs suffix-segs) (split-name-dots ".bashrc")))
  (check stem-segs => '(".bashrc"))
  (check suffix-segs => '())
) ;let-values

;; "." 与 ".." 整体作 stem
(let-values (((stem-segs _) (split-name-dots ".")))
  (check stem-segs => '("."))
) ;let-values
(let-values (((stem-segs _) (split-name-dots "..")))
  (check stem-segs => '(".."))
) ;let-values

;; 末尾点：空后缀
(let-values (((stem-segs suffix-segs) (split-name-dots "foo.")))
  (check stem-segs => '("foo"))
  (check suffix-segs => '("."))
) ;let-values

(check-report)

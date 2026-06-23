(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-suffixes
;; 获取末段的所有后缀(对齐 pathlib .suffixes)。
;;
;; 语法
;; ----
;; (path-suffixes path-value) -> vector
;;
;; 参数
;; ----
;; path-value : path-value
;;
;; 返回值
;; ------
;; vector
;; 末段按 "." 切分后,从第二个非首段起的每个后缀(含前导 ".")。
;; 隐藏文件(.bashrc)、无后缀文件返回 #()。

;; 多后缀
(check (path-suffixes (path "archive.tar.gz")) => #(".tar" ".gz"))
;; 单后缀
(check (path-suffixes (path "demo.txt")) => #(".txt"))
;; 无后缀
(check (path-suffixes (path "README")) => #())
;; 隐藏文件(首段为空)无后缀
(check (path-suffixes (path ".bashrc")) => #())
;; 多点文件 a.b.c → (.b .c),首段 a 不算
(check (path-suffixes (path "a.b.c")) => #(".b" ".c"))
;; 带目录的多后缀
(check (path-suffixes (path "/tmp/x.tar.gz")) => #(".tar" ".gz"))
;; 连续点 a..b → (. .b)(对齐 pathlib: ['.', '.b'])
(check (path-suffixes (path "a..b")) => #("." ".b"))

(when (os-windows?)
  (check (path-suffixes (path "C:\\Users\\foo\\bar.tar.gz")) => #(".tar" ".gz"))
) ;when

(check-report)

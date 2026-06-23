(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-drive
;; 获取路径的驱动器字母。
;;
;; 语法
;; ----
;; (path-drive path-value)
;;
;; 参数
;; ----
;; path-value : path-value
;; 要查询的路径值。
;;
;; 返回值
;; ----
;; string
;; 返回驱动器字母，如 "C:" (带冒号, 对齐 pathlib.PurePath.drive)；
;; UNC 路径返回 "\\server\share" 整体 anchor (无冒号)；
;; 非 Windows 路径、Windows 相对路径 / current-drive root 返回空字符串。

(check (path-drive (path "/")) => "")
(check (path-drive (path "/a")) => "")
(check (path-drive (path "a/b")) => "")

(when (os-windows?)
  ;; drive-absolute: 单字符盘符 + 冒号
  (check (path-drive (path "C:\\a")) => "C:")
  (check (path-drive (path "C:\\a\\b")) => "C:")
  (check (path-drive (path "C:\\foo")) => "C:")
  (check (path-drive (path "C:\\")) => "C:")
  (check (path-drive (path "C:/Users/foo")) => "C:")
  (check (path-drive (path "C:/a/b")) => "C:")
  ;; drive-relative: 同样带冒号
  (check (path-drive (path "C:foo")) => "C:")
  (check (path-drive (path "C:foo\\bar")) => "C:")
  ;; path-of-drive 构造的根: drive 字段是单字符,访问器返回带冒号
  (check (path-drive (path-of-drive #\c)) => "C:")
  ;; current-drive root / 相对: 空
  (check (path-drive (path "\\foo")) => "")
  (check (path-drive (path "\\")) => "")
  (check (path-drive (path "foo\\bar")) => "")
  ;; UNC: drive 字段存 \\server\share anchor(无冒号)
  (check (path-drive (path "\\\\srv")) => "\\\\srv")
  (check (path-drive (path "\\\\srv\\sh")) => "\\\\srv\\sh")
  (check (path-drive (path "\\\\srv\\sh\\a")) => "\\\\srv\\sh")
  (check (path-drive (path "\\\\srv\\share\\a\\b")) => "\\\\srv\\share")
  (check (path-drive (path "\\\\srv\\share")) => "\\\\srv\\share")
) ;when

(check-report)

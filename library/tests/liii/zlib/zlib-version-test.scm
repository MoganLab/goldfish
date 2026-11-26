;; zlib-version
;; 返回链接到 Goldfish 的 zlib 库版本字符串。
;;
;; 语法
;; ----
;; (zlib-version)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; -----
;; string?
;; zlib 库的版本字符串，与构建时 pin 的 zlib 源码版本一致。
;;
;; 说明
;; ----
;; 1. Goldfish 显式 pin zlib v1.3.2（与 mogan 解析到的 zlib 版本一致），
;;    因此返回值固定为 "1.3.2"。
;; 2. zlib 静态链接，返回值即编译期 ZLIB_VERSION 宏。
;; 3. 可用于在运行期核对实际链接的 zlib 版本。

(import (liii check) (liii zlib))

(check-set-mode! 'report-failed)

(check (zlib-version) => "1.3.2")
(check (string? (zlib-version)) => #t)

(check-report)

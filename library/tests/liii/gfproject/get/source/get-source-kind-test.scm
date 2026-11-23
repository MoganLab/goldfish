;; get-source-kind
;; 区分 gf get 来源：本地路径、仓库 URL、name@organization 短包名。
;;
;; 语法
;; ----
;; (get-source-kind arg)
;;
;; 参数
;; ----
;; arg : string?
;; 非空来源字符串。
;;
;; 返回值
;; ----
;; symbol
;; 'path、'url 或 'package；无法解析时抛 value-error。

(import (liii check) (liii gfproject get source))

(check-set-mode! 'report-failed)

;; 本地路径
(for-each
  (lambda (s) (check (get-source-kind s) => 'path))
  '("~/git/gf-highlight" "./repo" "../repo" "/tmp/repo" "C:/git/repo" "C:\\git\\repo")
) ;for-each

;; 仓库 URL（含 SCP 风格）
(for-each
  (lambda (s) (check (get-source-kind s) => 'url))
  '("https://gitee.com/MoganLab/gf-highlight"
    "file:///tmp/repo.git"
    "git@gitee.com:MoganLab/gf-highlight.git")
) ;for-each

;; 短包名
(check (get-source-kind "highlight@liii") => 'package)
(check (get-source-kind "pi@liii") => 'package)

;; 无法解析
(check-catch 'value-error (get-source-kind "plain-name"))
(check-catch 'type-error (get-source-kind ""))
(check-catch 'type-error (get-source-kind 42))

(check-report)

;; hint-roots-of
;; 解析给定 hint 文件路径所关联的 hint 根目录列表。
;;
;; 语法
;; ----
;; (hint-roots-of hint-file)
;;
;; 参数
;; ----
;; hint-file : string  hint 库文件路径
;;
;; 返回值
;; ------
;; list of string
;; 关联的 hint 根目录路径列表。

(import (scheme base) (liii check) (liii goldcheck type-hint env))

(check-set-mode! 'report-failed)

;; 1. goldfish/hint 下的文件
(let ((roots (hint-roots-of "goldfish/hint/liii/clean.scm")))
  (check roots => '("goldfish/hint"))
) ;let

;; 2. tools 下的文件关联自身 hint 根及全局 goldfish/hint
(let ((roots (hint-roots-of "tools/check/hint/liii/foo.scm")))
  (check roots => '("tools/check/hint" "goldfish/hint"))
) ;let

;; 3. 非 hint 路径返回空列表
(check (hint-roots-of "other/path/foo.scm") => '())

(check-report)

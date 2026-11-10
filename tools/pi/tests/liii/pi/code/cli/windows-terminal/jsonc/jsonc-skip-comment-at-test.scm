;; jsonc-skip-comment-at
;; 跳过指定位置的 JSONC 行注释或块注释。
;;
;; 语法
;; ----
;; (jsonc-skip-comment-at text index len) -> integer | #f
;;
;; 参数
;; ----
;; text : string; index : integer; len : integer
;; 待扫描文本、当前位置和扫描上限。
;;
;; 返回值
;; ----
;; integer | #f
;; 返回注释后的位置；当前位置不是注释时返回 #f。
;;

(import (scheme base) (liii check) (liii pi code cli windows-terminal jsonc))

(check-set-mode! 'report-failed)

(check (jsonc-skip-comment-at "// note\nkey" 0 10) => 8)
(check (jsonc-skip-comment-at "/* note */key" 0 13) => 10)
(check-false (jsonc-skip-comment-at "/key" 0 4))
(check-false (jsonc-skip-comment-at "key" 0 3))

(check-report)

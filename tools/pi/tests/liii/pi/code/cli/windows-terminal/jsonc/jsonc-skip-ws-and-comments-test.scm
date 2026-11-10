;; jsonc-skip-ws-and-comments
;; 连续跳过 JSONC 空白、行注释和块注释。
;;
;; 语法
;; ----
;; (jsonc-skip-ws-and-comments text start len) -> integer
;;
;; 参数
;; ----
;; text : string; start : integer; len : integer
;; 待扫描文本、起始位置和扫描上限。
;;
;; 返回值
;; ----
;; integer
;; 返回首个非空白且非注释字符的位置。
;;

(import (scheme base) (liii check) (liii pi code cli windows-terminal jsonc))

(check-set-mode! 'report-failed)

(let* ((text " \t/* note */ // line\n  key")
       (index (jsonc-skip-ws-and-comments text 0 (string-length text)))
      ) ;
  (check (string-ref text index) => #\k)
) ;let*
(check (jsonc-skip-ws-and-comments " \n" 0 2) => 2)

(check-report)

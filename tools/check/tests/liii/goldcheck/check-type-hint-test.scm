;; check-type-hint
;; 校验单个 hint 库文件的合法性（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (check-type-hint hint-file)
;;
;; 参数
;; ----
;; hint-file : string   类型提示库文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，合规时返回空列表。

(import (scheme base)
  (liii check)
  (liii path)
  (liii gfproject config)
  (liii goldcheck)
) ;import

(check-set-mode! 'report-failed)

;; 1. 检查真实存在的合规 hint 库
(let* ((root (gfproject-root))
       (path-str (if (and root (string? root))
                   (path->string (path-join root "goldfish" "hint" "scheme" "file.scm"))
                   "../../goldfish/hint/scheme/file.scm"
                 ) ;if
       ) ;path-str
      ) ;
  (check (check-type-hint path-str) => '())
) ;let*

(check-report)

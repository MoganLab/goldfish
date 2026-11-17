;; *html-entities*
;; HTML5 命名字符实体映射表。
;;
;; 语法
;; ----
;; *html-entities*
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; list
;; 存储全部 HTML5 命名实体到 Unicode 码点映射的关联列表。

(import (scheme base))
(import (liii check))
(import (liii cmark html-entities))

(check-set-mode! 'report-failed)

(check-true (pair? *html-entities*))
(check (assoc 'amp *html-entities*) => '(amp 38))
(check (assoc 'lt *html-entities*) => '(lt 60))

(check-report)

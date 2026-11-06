;; parse-results-position
;; 读取解析结果流的位置。
;;
;; 语法
;; ----
;; (parse-results-position results)
;;
;; 参数
;; ----
;; results : <parse-results>
;; 解析结果流记录。
;;
;; 返回值
;; ----
;; <parse-position> 或 #f
;; 当前位置；未知时为 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let* ((pos (make-parse-position "test" 3 7))
       (gen (lambda () (values pos #f)))
       (results (base-generator->results gen))
      ) ;
  (check (parse-results-position results) => pos)
) ;let*

(check-report)

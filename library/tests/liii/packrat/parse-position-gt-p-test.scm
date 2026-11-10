;; parse-position>?
;; 比较两个解析位置的先后。
;;
;; 语法
;; ----
;; (parse-position>? a b)
;;
;; 参数
;; ----
;; a : <parse-position> 或 #f
;; 左侧位置。
;; b : <parse-position> 或 #f
;; 右侧位置。
;;
;; 返回值
;; ----
;; boolean
;; a 严格晚于 b 时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 先比较行号，行号相同时比较列号；
;; a 为 #f 时返回 #f，b 为 #f 时返回 #t。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

;; 行号比较
(check (parse-position>? (make-parse-position "f" 2 0) (make-parse-position "f" 1 0))
  =>
  #t
) ;check
(check (parse-position>? (make-parse-position "f" 1 0) (make-parse-position "f" 2 0))
  =>
  #f
) ;check

;; 行号相同时比较列号
(check (parse-position>? (make-parse-position "f" 1 5) (make-parse-position "f" 1 0))
  =>
  #t
) ;check
(check (parse-position>? (make-parse-position "f" 1 0) (make-parse-position "f" 1 5))
  =>
  #f
) ;check

;; 相同位置不算靠后
(check (parse-position>? (make-parse-position "f" 1 0) (make-parse-position "f" 1 0))
  =>
  #f
) ;check

;; #f 处理
(check (parse-position>? #f (make-parse-position "f" 1 0)) => #f)
(check (parse-position>? (make-parse-position "f" 1 0) #f) => #t)

(check-report)

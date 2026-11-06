;; tzif-transition
;; 构造一条 TZif 本地时间类型转移记录。
;;
;; 语法
;; ----
;; (tzif-transition time offset dst? designation standard? universal-time?)
;;
;; 参数
;; ----
;; time : integer? 转移时刻。
;; offset : integer? UTC 偏移秒数。
;; dst? : boolean? 是否夏令时。
;; designation : string? 时区缩写。
;; standard? : boolean? 是否本地标准时间。
;; universal-time? : boolean? 是否按 UTC 解释。
;;
;; 返回值
;; ----
;; tzif-transition? 新建的转移记录。

(import (liii check) (liii date-time internal tzif))

(check-set-mode! 'report-failed)

(define tr (tzif-transition 100 3600 #t "EDT" #f #f))

(check-true (tzif-transition? tr))
(check (tzif-transition-time tr) => 100)
(check (tzif-transition-designation tr) => "EDT")

(check-report)

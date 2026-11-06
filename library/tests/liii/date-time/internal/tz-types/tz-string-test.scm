;; tz-string
;; 构造 POSIX TZ 字符串解析结果记录。
;;
;; 语法
;; ----
;; tz-string stdname stdoffset dstname dstoffset change-to-dst change-to-std
;;
;; 参数
;; ----
;; stdname : string? 标准时间缩写。
;; stdoffset : tz-offset? 标准时间偏移。
;; dstname : string? 夏令时缩写。
;; dstoffset : tz-offset? 夏令时偏移。
;; change-to-dst : tz-transition? 进入夏令时的转移。
;; change-to-std : tz-transition? 退出夏令时的转移。
;;
;; 返回值
;; ----
;; tz-string? 新建的 POSIX TZ 定义记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
) ;define

(check-true (tz-string? ts))

(check-report)

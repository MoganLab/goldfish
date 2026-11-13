;; fuzzy-filter
;; 根据模糊匹配查询过滤并排序列表条目。
;;
;; 语法
;; ----
;; (fuzzy-filter items query get-text)
;;
;; 参数
;; ----
;; items : list
;; 待过滤条目的列表。
;;
;; query : string
;; 搜索查询字符串，支持多词（以空格或 / 分隔）及字母数字颠倒匹配。
;;
;; get-text : procedure
;; 从单个条目中提取待匹配文本的过程，类型为 (item -> string)。
;;
;; 返回值
;; ----
;; list
;; 匹配的条目列表，按匹配相关度从高到低排序。

(import (scheme base) (liii check) (liii tui fuzzy))

(check-set-mode! 'report-failed)

;; 空查询保留原顺序；最佳匹配排在前面。
(check (fuzzy-filter '("xhigh" "high" "low") "" (lambda (x) x))
  =>
  '("xhigh" "high" "low")
) ;check
(check (fuzzy-filter '("xhigh" "high" "low") "high" (lambda (x) x))
  =>
  '("high" "xhigh")
) ;check

;; 空白和斜杠分隔的 token 必须全部匹配。
(check (fuzzy-filter '("off" "Moderate reasoning" "Deep reasoning")
         "moderate/reasoning"
         (lambda (x) x)
       ) ;fuzzy-filter
  =>
  '("Moderate reasoning")
) ;check
(check (fuzzy-filter '("off" "minimal" "low") "zzz" (lambda (x) x)) => '())

;; 与上游一致，字母数字相邻查询支持交换匹配，并施加较小惩罚。
(check (fuzzy-filter '("gpt5" "other") "5gpt" (lambda (x) x)) => '("gpt5"))
(check (fuzzy-filter '("5gpt" "other") "gpt5" (lambda (x) x)) => '("5gpt"))

;; 重复字符必须按顺序匹配，查询长于文本时直接排除。
(check (fuzzy-filter '("abc" "ac" "axbyc") "abc" (lambda (x) x))
  =>
  '("abc" "axbyc")
) ;check
(check (fuzzy-filter '("abc" "ab") "abcd" (lambda (x) x)) => '())

;; 连续空白和斜杠视为同一组分隔符。
(check (fuzzy-filter '("alpha beta" "alpha gamma" "beta only")
         " / alpha // beta "
         (lambda (x) x)
       ) ;fuzzy-filter
  =>
  '("alpha beta")
) ;check

;; get-text 支持非字符串条目，过滤与排序后保留原条目。
(let ((items '(("low" . 1) ("high" . 2) ("xhigh" . 3))))
  (check (fuzzy-filter items "high" car) => '(("high" . 2) ("xhigh" . 3)))
) ;let

;; 含非法 UTF-8 字节序列的文本与查询不崩溃，合法文本仍正常过滤。
(let ((bad (string #\b #\a (integer->char #x80) #\d))
      (bad-q (string #\d (integer->char #xfe)))
     ) ;
  (check (fuzzy-filter (list "debug" bad "other") "debug" (lambda (x) x)) => '("debug"))
  (check (fuzzy-filter (list "debug" bad) "bd" (lambda (x) x)) => (list bad))
  (check (fuzzy-filter (list "debug" "other") bad-q (lambda (x) x)) => '())
) ;let

(check-report)

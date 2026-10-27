(import (scheme base)
  (liii check)
  (liii tui)
) ;import

(check-set-mode! 'report-failed)

;; 空查询保留原顺序；最佳匹配排在前面。
(check (fuzzy-filter '("xhigh" "high" "low") "" (lambda (x) x))
  => '("xhigh" "high" "low"))
(check (fuzzy-filter '("xhigh" "high" "low") "high" (lambda (x) x))
  => '("high" "xhigh"))

;; 空白和斜杠分隔的 token 必须全部匹配。
(check (fuzzy-filter '("off" "Moderate reasoning" "Deep reasoning") "moderate/reasoning"
         (lambda (x) x))
  => '("Moderate reasoning"))
(check (fuzzy-filter '("off" "minimal" "low") "zzz" (lambda (x) x)) => '())

;; 与上游一致，字母数字相邻查询支持交换匹配，并施加较小惩罚。
(check (fuzzy-filter '("gpt5" "other") "5gpt" (lambda (x) x)) => '("gpt5"))
(check (fuzzy-filter '("5gpt" "other") "gpt5" (lambda (x) x)) => '("5gpt"))

;; 重复字符必须按顺序匹配，查询长于文本时直接排除。
(check (fuzzy-filter '("abc" "ac" "axbyc") "abc" (lambda (x) x))
  => '("abc" "axbyc"))
(check (fuzzy-filter '("abc" "ab") "abcd" (lambda (x) x)) => '())

;; 连续空白和斜杠视为同一组分隔符。
(check (fuzzy-filter '("alpha beta" "alpha gamma" "beta only") " / alpha // beta "
         (lambda (x) x))
  => '("alpha beta"))

;; get-text 支持非字符串条目，过滤与排序后保留原条目。
(let ((items '(("low" . 1) ("high" . 2) ("xhigh" . 3))))
  (check (fuzzy-filter items "high" car) => '(("high" . 2) ("xhigh" . 3))))

(check-report)

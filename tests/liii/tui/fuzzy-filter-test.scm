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

(check-report)

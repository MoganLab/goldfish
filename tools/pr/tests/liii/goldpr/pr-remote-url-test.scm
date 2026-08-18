(import (liii check) (liii string) (liii goldpr))

(check-set-mode! 'report-failed)

;; pr-remote-url
;; 获取当前仓库 origin 远程的 URL，gf pr 从该远程拉取 PR。
;;
;; 语法
;; ----
;; (pr-remote-url)
;;
;; 返回值
;; ----
;; string? 或 #f
;; origin 远程存在时返回其 URL 字符串（去除首尾空白），否则返回 #f。

;; 本仓库（goldfish）配置有 origin 远程
(check (string? (pr-remote-url)) => #t)
(check (> (string-length (pr-remote-url)) 0) => #t)

;; URL 不含首尾空白
(check (pr-remote-url) => (string-trim-both (pr-remote-url)))

;; 不存在的远程返回 #f
(check (pr-remote-url "no-such-remote") => #f)

(check-report)

;; pi-format-context-usage
;; 将上下文占用格式化为页脚左下角的描述串（对齐上游 pi 页脚：
;; "x.x%/1.0M (auto)"；压缩后尚无新响应时显示 "?"）
;;
;; 语法
;; ----
;; (pi-format-context-usage ctx . opt-auto)
;;
;; 参数
;; ----
;; ctx : pi-context-usage
;; 上下文占用 record。
;; opt-auto : boolean (可选)
;; 是否附加 " (auto)" 标记，缺省为 #t。
;;
;; 返回值
;; ----
;; string
;; 形如 "12.3%/1.0M (auto)" 的描述串。

(import (scheme base)
  (liii check)
  (liii pi code core context-usage)
) ;import

(check-set-mode! 'report-failed)

;; 1. 已知占比：百分比 + 窗口容量
(check (pi-format-context-usage (make-pi-context-usage 12.3 1048576 #t))
  => "12.3%/1.0M (auto)")

;; 2. 压缩后尚无新响应：显示 "?"
(check (pi-format-context-usage (make-pi-context-usage #f 1048576 #f))
  => "?/1.0M (auto)")

;; 3. 窗口容量按 k/M 缩写
(check (pi-format-context-usage (make-pi-context-usage 1.5 200000 #t))
  => "1.5%/200k (auto)")

;; 4. 窗口缺失时容量退化为 0
(check (pi-format-context-usage (make-pi-context-usage 0.0 #f #t))
  => "0.0%/0 (auto)")

;; 5. 不带 (auto) 标记
(check (pi-format-context-usage (make-pi-context-usage 12.3 1048576 #t) #f)
  => "12.3%/1.0M")

(check-report)

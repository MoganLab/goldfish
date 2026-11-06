;; native-clipboard-image-path
;; 从外部系统剪贴板读取图片并保存为临时文件路径，支持注入 runner 进行测试。
;;
;; 语法
;; ----
;; (native-clipboard-image-path [runner])
;;
;; 参数
;; ----
;; runner : procedure（可选）
;; 无参过程，返回图片路径或 #f。
;;
;; 返回值
;; ----
;; string 或 #f
;; 图片文件的本地绝对路径；剪贴板无图片或读取失败时返回 #f。

(import (scheme base)
  (liii check)
  (liii tui clipboard native)
) ;import

(check-set-mode! 'report-failed)

;; 1. 注入有效图片路径时正确返回
(check (native-clipboard-image-path (lambda () "/tmp/mock-clip-image.png"))
  =>
  "/tmp/mock-clip-image.png"
) ;check

;; 2. 剪贴板无图片时返回 #f
(check-false (native-clipboard-image-path (lambda () #f)))

;; 3. 生产入口调用不崩溃（返回 string? 或 #f）
(let ((res (native-clipboard-image-path)))
  (check-true (or (string? res) (not res)))
) ;let

(check-report)

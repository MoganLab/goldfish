;; clipboard-copy!
;; 将文本复制到终端（通过 OSC 52 转义序列）及系统原生剪贴板。
;;
;; 语法
;; ----
;; (clipboard-copy! [terminal] text)
;;
;; 参数
;; ----
;; terminal : terminal? (可选)
;; 目标终端实例。若提供，则通过 terminal-write 写入并刷新；若未提供，则向标准输出输出。
;;
;; text : string
;; 欲复制到剪贴板的文本内容。
;;
;; 返回值
;; ----
;; boolean
;; 系统原生剪贴板写入是否成功。

(import (scheme base) (liii check) (liii tui terminal) (liii tui clipboard))

(check-set-mode! 'report-failed)

;; 1. virtual-terminal 写入验证
(let ((term (make-virtual-terminal :width 80 :height 24)))
  (let ((res (clipboard-copy! term "test copy")))
    (check-true (boolean? res))
    (check-true (pair? (member (osc52-sequence "test copy") (virtual-terminal-raw term)))
    ) ;check-true
  ) ;let
) ;let

;; 2. 无 terminal 参数形式（输出到标准输出，测试环境不抛出异常）
(check-true (boolean? (clipboard-copy! "test copy no term")))

(check-report)

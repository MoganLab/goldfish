;; %make-editor-data
;; 创建编辑器内部数据记录实例。
;;
;; 语法
;; ----
;; (%make-editor-data buffer history multiline? focused? placeholder
;;   cursor-style on-submit on-change top-border bottom-border border-color
;;   submit-on-enter? max-lines top-row pastes undo-stack last-action)
;;
;; 参数
;; ----
;; buffer : any
;; 缓冲区。
;;
;; history : any
;; 历史。
;;
;; multiline? : boolean?
;; 多行模式。
;;
;; focused? : boolean?
;; 聚焦。
;;
;; placeholder : string?
;; 占位符。
;;
;; cursor-style : symbol?
;; 光标样式。
;;
;; on-submit : any
;; 提交回调。
;;
;; on-change : any
;; 变更回调。
;;
;; top-border : any
;; 上边框。
;;
;; bottom-border : any
;; 下边框。
;;
;; border-color : any
;; 边框颜色。
;;
;; submit-on-enter? : any
;; 回车提交。
;;
;; max-lines : any
;; 最大行数。
;;
;; top-row : integer?
;; 视口行。
;;
;; pastes : any
;; 粘贴表。
;;
;; undo-stack : list?
;; 撤销栈。
;;
;; last-action : any
;; 上次操作。
;;
;; 返回值
;; ----
;; %editor-data?
;; 编辑器内部数据记录。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (check-true (%editor-data? d))
) ;let

(check-report)

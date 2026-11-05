;; %repl-refresh!
;; 把滚动区、输入行与附加行渲染到屏幕。
;;
;; 语法
;; ----
;; (%repl-refresh! screen term scroll-lines ed width pending? . opt-extra-lines)
;;
;; 参数
;; ----
;; screen : screen
;; 主屏幕对象。
;; term : terminal
;; 终端对象。
;; scroll-lines : list
;; 已经滚动的输出行列表。
;; ed : editor
;; 编辑器对象。
;; width : integer
;; 终端宽度。
;; pending? : boolean
;; 为真时表示正在续写多行输入。
;; opt-extra-lines : list
;; 可选的附加行列表，例如补全菜单。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未定义，函数通过渲染屏幕生效。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii tui)
  (liii goldrepl render)
) ;import

(check-set-mode! 'report-failed)

;; 只渲染输入行
(let* ((vt (make-virtual-terminal :width 60 :height 10))
       (screen (make-main-screen vt))
       (ed (make-text-input "string->"))
      ) ;
  (%repl-refresh! screen vt '() ed 60 #f '())
  (check (length (screen-lines screen)) => 1)
  (check-true (string-contains (car (screen-lines screen)) "string->"))
) ;let*

;; 附加行渲染在输入行下方
(let* ((vt (make-virtual-terminal :width 60 :height 10))
       (screen (make-main-screen vt))
       (ed (make-text-input "string->"))
      ) ;
  (%repl-refresh! screen vt '() ed 60 #f '(" 1 string->list"))
  (check (length (screen-lines screen)) => 2)
  (check-true (string-contains (cadr (screen-lines screen)) "string->list"))
) ;let*

;; 滚动区的内容渲染在输入行上方
(let* ((vt (make-virtual-terminal :width 60 :height 10))
       (screen (make-main-screen vt))
       (ed (make-text-input "abc"))
      ) ;
  (%repl-refresh! screen vt '("output line") ed 60 #f '())
  (check (length (screen-lines screen)) => 2)
  (check-true (string-contains (car (screen-lines screen)) "output line"))
) ;let*

(check-report)

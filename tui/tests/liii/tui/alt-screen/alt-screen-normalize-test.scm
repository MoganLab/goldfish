;; alt-screen-normalize
;; 归一化整帧内容：按终端宽度裁剪每行，并补齐到终端高度。
;; 不写入终端，仅返回归一化后的帧行列表。
;;
;; 语法
;; ----
;; (alt-screen-normalize screen lines)
;;
;; 参数
;; ----
;; screen : alt-screen?
;; 备用屏驱动实例。
;;
;; lines : list?
;; 待归一化的文本行字符串列表。
;;
;; 返回值
;; ----
;; list?
;; 归一化后的帧行字符串列表，长度等于终端高度。

(import (scheme base) (liii check) (liii tui alt-screen) (liii tui terminal))

(check-set-mode! 'report-failed)

;; 1. 按宽度裁剪、按高度补齐
(let* ((vt (make-virtual-terminal :width 5 :height 3)) (screen (make-alt-screen vt)))
  (check (alt-screen-normalize screen '("ab" "cdefgh")) => '("ab" "cdefg" ""))
) ;let*

;; 2. 行数超过终端高度时裁剪到高度
(let* ((vt (make-virtual-terminal :width 5 :height 2)) (screen (make-alt-screen vt)))
  (check (alt-screen-normalize screen '("a" "b" "c")) => '("a" "b"))
) ;let*

;; 3. 内嵌换行的行按换行拆分
(let* ((vt (make-virtual-terminal :width 8 :height 3)) (screen (make-alt-screen vt)))
  (check (alt-screen-normalize screen '("a\nb")) => '("a" "b" ""))
) ;let*

;; 4. 归一化不改变备用屏当前帧
(let* ((vt (make-virtual-terminal :width 5 :height 3)) (screen (make-alt-screen vt)))
  (alt-screen-normalize screen '("ab" "cdefgh"))
  (check (alt-screen-lines screen) => '())
) ;let*

;; 5. 类型检查
(check-catch 'type-error (alt-screen-normalize "not-a-screen" '("a")))

(check-report)

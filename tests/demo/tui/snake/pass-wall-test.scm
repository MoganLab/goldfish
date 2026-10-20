(import (liii check)
  (demo tui snake logic)
) ;import

(check-set-mode! 'report-failed)

;; snake-pass-wall-test：验证四面墙壁穿越逻辑（刚好 10 组针对性测试用例）

;; 1. 正常内部点无越界
(check (snake-pass-wall 5 5 20 12) => '(5 5))

;; 2. 向右穿出右边界：x = 20 穿越回 x = 0
(check (snake-pass-wall 20 5 20 12) => '(0 5))

;; 3. 刚好在右边界内沿：x = 19 保持 19
(check (snake-pass-wall 19 5 20 12) => '(19 5))

;; 4. 向左穿出左边界：x = -1 穿越至右侧对侧 x = 19
(check (snake-pass-wall -1 5 20 12) => '(19 5))

;; 5. 多步向左越界：x = -2 穿越至 x = 18
(check (snake-pass-wall -2 3 20 12) => '(18 3))

;; 6. 向上穿出上边界：y = -1 穿越至底侧对侧 y = 11
(check (snake-pass-wall 8 -1 20 12) => '(8 11))

;; 7. 多步向上越界：y = -3 穿越至 y = 9
(check (snake-pass-wall 8 -3 20 12) => '(8 9))

;; 8. 向下穿出下边界：y = 12 穿越至顶侧对侧 y = 0
(check (snake-pass-wall 8 12 20 12) => '(8 0))

;; 9. 刚好在底边界内沿：y = 11 保持 11
(check (snake-pass-wall 8 11 20 12) => '(8 11))

;; 10. 对角双向同时穿墙：左上角 (-1, -1) 环绕至右下角 (19, 11)
(check (snake-pass-wall -1 -1 20 12) => '(19 11))

(check-report)

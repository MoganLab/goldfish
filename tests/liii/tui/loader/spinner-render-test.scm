(import (liii check)
        (liii tui component)
        (liii tui text-width)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; spinner-render 渲染输出与宽度自适应测试

;; 1. 标准单行渲染格式 [frame] [text]
(let ((s (make-spinner "Loading..." :frames 'line)))
  (let ((lines (component-render s 80)))
    (check (length lines) => 1)
    (check (car lines) => "- Loading..."))
  ;; 推进一帧后渲染更新
  (spinner-tick! s)
  (check (component-render s 80) => '("\\ Loading...")))

;; 2. 前缀与后缀组合渲染
(let ((s (make-spinner "Downloading"
                       :frames 'line
                       :prefix "[App] "
                       :suffix " 45%")))
  (check (component-render s 80) => '("[App] - Downloading 45%")))

;; 3. 仅有转轮字符（无文本、无前后缀）
(let ((s (make-spinner :frames 'line)))
  (check (component-render s 80) => '("-")))

;; 4. 完成状态与图标渲染
(let ((s (make-spinner "Task" :frames 'line)))
  (spinner-success! s "Task finished successfully")
  (check (component-render s 80) => '("✔ Task finished successfully")))

;; 5. 宽度受限时的安全截断（不得超出 width）
(let* ((s (make-spinner "Very long task description for testing width limits"
                        :frames 'line
                        :prefix ">> "))
       (w 15)
       (lines (component-render s w)))
  (check (length lines) => 1)
  (check (<= (display-width (car lines)) w) => #t))

;; 6. 极端小宽度边界 (width <= 0 或 width = 1)
(let ((s (make-spinner "Test" :frames 'line)))
  (check (component-render s 0) => '(""))
  (let ((lines1 (component-render s 1)))
    (check (length lines1) => 1)
    (check (<= (display-width (car lines1)) 1) => #t)))

;; 7. 隐藏状态渲染返回空列表
(let ((s (make-spinner "Hidden" :visible #f)))
  (check (component-render s 80) => '()))

;; 8. 错误处理：非法 width
(let ((s (make-spinner)))
  (check-catch 'type-error (component-render s "not-a-number")))

(check-report)

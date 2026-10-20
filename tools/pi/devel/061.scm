(import (liii check) (liii tui component) (liii tui keys) (liii tui editor))

(check-set-mode! 'report-failed)

;; 任务 061: (liii tui editor) 自动化验收脚本
;; 通过转义序列解析 + 按键回放全流程非交互验证

;; 1. 单行 TextInput 交互验证：按键序列输入、光标移动、退格与反显光标渲染
(let ((ti (make-text-input :cursor-style 'bar)))
  ;; 模拟连续按键输入 "hello"
  (for-each (lambda (k) (component-handle-input ti k)) (parse-keys "hello"))
  (check (editor-text ti) => "hello")
  (check (editor-cursor ti) => '(0 5))

  ;; 模拟按左方向键两次
  (component-handle-input ti (parse-key "\x1b;[D"))
  (component-handle-input ti (parse-key "\x1b;[D"))
  (check (editor-cursor ti) => '(0 3))

  ;; 插入下划线
  (component-handle-input ti "_")
  (check (editor-text ti) => "hel_lo")
  (check (editor-cursor ti) => '(0 4))

  ;; 按 Home 键跳到行首
  (component-handle-input ti (parse-key "\x1b;[H"))
  (check (editor-cursor ti) => '(0 0))

  ;; 在行首插入 ">>"
  (for-each (lambda (k) (component-handle-input ti k)) (parse-keys ">>"))
  (check (editor-text ti) => ">>hel_lo")
  (check (editor-cursor ti) => '(0 2))

  ;; 按 End 键跳到行尾 (">>hel_lo" 长度为 8)
  (component-handle-input ti (parse-key "\x1b;[F"))
  (check (editor-cursor ti) => '(0 8))

  ;; 按退格键
  (component-handle-input ti (parse-key ""))
  (check (editor-text ti) => ">>hel_l")
  (check (editor-cursor ti) => '(0 7))

  ;; 验证渲染输出
  (check (component-render ti 20) => '(">>hel_l|"))
) ;let

;; 2. 历史记录导航验证
(let ((ti (make-text-input)))
  ;; 输入并提交第 1 条
  (for-each (lambda (k) (component-handle-input ti k)) (parse-keys "echo 1"))
  (component-handle-input ti (parse-key "\r\n"))
  (editor-set-text! ti "")

  ;; 输入并提交第 2 条
  (for-each (lambda (k) (component-handle-input ti k)) (parse-keys "echo 2"))
  (component-handle-input ti (parse-key "\r\n"))
  (editor-set-text! ti "")

  ;; 正在编辑草稿
  (for-each (lambda (k) (component-handle-input ti k)) (parse-keys "draft..."))

  ;; 向上翻历史 (Up)
  (component-handle-input ti (parse-key "\x1b;[A"))
  (check (editor-text ti) => "echo 2")

  (component-handle-input ti (parse-key "\x1b;[A"))
  (check (editor-text ti) => "echo 1")

  ;; 向下翻回草稿 (Down)
  (component-handle-input ti (parse-key "\x1b;[B"))
  (check (editor-text ti) => "echo 2")

  (component-handle-input ti (parse-key "\x1b;[B"))
  (check (editor-text ti) => "draft...")
) ;let

;; 3. 多行 TextArea 换行与上下移动验证
(let ((ta (make-text-area "line1")))
  ;; 回车换行
  (component-handle-input ta (parse-key "\r\n"))
  (check (editor-lines ta) => '("line1" ""))
  (check (editor-cursor ta) => '(1 0))

  ;; 输入第二行内容
  (for-each (lambda (k) (component-handle-input ta k)) (parse-keys "second line"))
  (check (editor-lines ta) => '("line1" "second line"))

  ;; 上移一行并吸附
  (component-handle-input ta (parse-key "\x1b;[A"))
  (check (editor-cursor ta) => '(0 5))

  ;; 下移回第二行
  (component-handle-input ta (parse-key "\x1b;[B"))
  (check (editor-cursor ta) => '(1 5))
) ;let

(check-report)

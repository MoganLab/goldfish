(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes config state)
  (liii pi code modes config layout)
) ;import

(check-set-mode! 'report-failed)

;; pi-config-render：ranger 风格三列纯文本渲染。
;; 输入 state + 光标/激活列 + 终端宽高，输出恰好 height 行字符串，
;; 每行可见宽度不超过 width；列宽不足时隐藏第三列。

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"))

(define DIR (string-append FIXTURE "/config-tui"))
(define st (pi-config-state DIR :env '()))

(define (plain s) (strip-ansi s))
(define (line-width s) (display-width (plain s)))

;; 1. 列宽：宽屏三等分（第一列 22，第二列 34，其余给预览）
(check (pi-config-column-widths 120) => '(22 34 64))
;; 2. 中等宽度：第三列变窄但保留
(check (pi-config-column-widths 80) => '(22 34 24))
;; 3. 窄屏：第三列宽度为 0（隐藏）
(check (pi-config-column-widths 56) => '(22 34 0))
(check (pi-config-column-widths 40) => '(20 20 0))

;; 4. 行数与宽度约束
(let ((lines (pi-config-render st (pi-config-cursor 1 0) 0 120 20)))
  (check (length lines) => 20)
  (check (every (lambda (l) (<= (line-width l) 120)) lines) => #t)
) ;let

;; 5. 三列内容：providers 标题 + 选中 provider 的模型 + 详情
(let ((text (plain (string-join (pi-config-render st (pi-config-cursor 1 0) 0 120 20) "\n"))))
  (check (string-contains text "providers 3/6 on") => #t)
  (check (string-contains text "kimi-coding") => #t)
  (check (string-contains text "k3.1-pro") => #t)
  (check (string-contains text "baseUrl") => #t)
  (check (string-contains text "auth.json") => #t)
  ;; 可用 ● / 不可用 × 标记：kimi-coding 可用，deepseek 不在白名单
  (check (string-contains text "● kimi-coding") => #t)
  (check (string-contains text "× deepseek") => #t)
  ;; 状态栏显示可用/总数
  (check (string-contains text "3/6 providers") => #t)
) ;let

;; 6. 光标移动改变预览内容：provider 索引 0（google）时不显示 k3.1-pro
(let ((text (plain (string-join (pi-config-render st (pi-config-cursor 0 0) 0 120 20) "\n"))))
  (check (string-contains text "custom-kimi") => #t)
  ;; 中列切到 google，右列标题不应再是 k3.1-pro（状态栏仍会显示默认模型）
  (check (string-contains text "models · google") => #t)
  (check (string-contains text "preview · k3.1-pro") => #f)
) ;let

;; 7. 窄屏隐藏第三列：不出现 baseUrl 详情
(let ((text (plain (string-join (pi-config-render st (pi-config-cursor 1 0) 0 56 20) "\n"))))
  (check (string-contains text "k3.1-pro") => #t)
  (check (string-contains text "baseUrl") => #f)
) ;let

;; 8. 激活列不同 → 渲染结果不同（高亮差异）
(let ((a (pi-config-render st (pi-config-cursor 1 0) 0 120 20))
      (b (pi-config-render st (pi-config-cursor 1 0) 1 120 20))
     ) ;
  (check (equal? a b) => #f)
) ;let

;; 9a. 选中行用紫色底整行高亮（ranger 风格），底色覆盖整列宽度
(let ((raw (string-join (pi-config-render st (pi-config-cursor 1 0) 0 120 20) "\n")))
  ;; 焦点列浓紫、非焦点列浅紫
  (check (string-contains raw "48;2;74;44;122") => #t)
  (check (string-contains raw "48;2;116;90;158") => #t)
) ;let

;; 9. 状态栏显示配置目录与计数
(let ((text (plain (string-join (pi-config-render st (pi-config-cursor 1 0) 0 120 20) "\n"))))
  (check (string-contains text DIR) => #t)
) ;let

;; 10. 光标夹紧：越界索引被夹到有效范围，不抛错
(check (pair? (pi-config-cursor 99 99)) => #t)
(let ((lines (pi-config-render st (pi-config-cursor 99 99) 0 120 20)))
  (check (length lines) => 20)
) ;let

(check-report)

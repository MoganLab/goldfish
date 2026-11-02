;; selection-extract-text
;; 从多行文本中按选区坐标切片提取纯文本（剔除 ANSI 码并修剪行尾空白）。
;;
;; 语法
;; ----
;; (selection-extract-text lines sel)
;;
;; 参数
;; ----
;; lines : (list-of string?)
;; 渲染帧或视口行列表。
;; sel : selection-range?
;; 选区范围对象。
;;
;; 返回值
;; -----
;; string?
;; 提取出的纯文本（以换行符连接），若未激活则返回空字符串。

(import (scheme base) (liii check) (liii pi code modes interactive selection))

(check-set-mode! 'report-failed)

;; 1. 单行切片提取与尾部空格剔除
(let* ((lines (list "hello world!        "))
       (sel (make-selection-range '(6 . 0) '(10 . 0) #t))
       (text (selection-extract-text lines sel))
      ) ;
  (check text => "world")
) ;let*

;; 2. 代码内部缩进保留
(let* ((lines (list "    (define (foo x)    "))
       (sel (make-selection-range '(0 . 0) '(18 . 0) #t))
       (text (selection-extract-text lines sel))
      ) ;
  (check text => "    (define (foo x)")
) ;let*

;; 3. 多行选区切片与换行连接
(let* ((lines (list "first line content    "
                "  second line indented"
                "third line ends here  "
              ) ;list
       ) ;lines
       (sel (make-selection-range '(6 . 0) '(9 . 2) #t))
       (text (selection-extract-text lines sel))
      ) ;
  (check text => "line content\n  second line indented\nthird line")
) ;let*

;; 4. ANSI 转义代码过滤（剥离样式输出纯文本）
(let* ((lines (list "\x1b;[32mhello\x1b;[0m \x1b;[1;34mworld\x1b;[0m     "))
       (sel (make-selection-range '(0 . 0) '(10 . 0) #t))
       (text (selection-extract-text lines sel))
      ) ;
  (check text => "hello world")
) ;let*

;; 5. 中文与宽字符视觉列切片与向前对齐保护
(let ((lines (list "你好世界!    ")))
  ;; [0, 3] 覆盖 你(0..1) 好(2..3)
  (let ((sel (make-selection-range '(0 . 0) '(3 . 0) #t)))
    (check (selection-extract-text lines sel) => "你好")
  ) ;let

  ;; [2, 5] 覆盖 好(2..3) 世(4..5)
  (let ((sel (make-selection-range '(2 . 0) '(5 . 0) #t)))
    (check (selection-extract-text lines sel) => "好世")
  ) ;let

  ;; [1, 5] 起始落在 你(0..1) 中间，向前对齐包含 你
  (let ((sel (make-selection-range '(1 . 0) '(5 . 0) #t)))
    (check (selection-extract-text lines sel) => "你好世")
  ) ;let

  ;; [2, 4] 结束落在 世(4..5) 中间，向前对齐包含 世
  (let ((sel (make-selection-range '(2 . 0) '(4 . 0) #t)))
    (check (selection-extract-text lines sel) => "好世")
  ) ;let
) ;let

;; 6. 未激活选区安全短路返回空字符串
(let* ((lines (list "some text here"))
       (sel (make-selection-range '(0 . 0) '(5 . 0) #f))
      ) ;
  (check (selection-extract-text lines sel) => "")
) ;let*

(check-report)

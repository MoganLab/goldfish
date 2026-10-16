(import (liii check) (liii path) (liii os))

(check-set-mode! 'report-failed)

;; path-read-lines
;; 按行读取文本文件，返回行字符串列表。
;;
;; 语法
;; ----
;; (path-read-lines path [keepends [offset [limit]]]) → list
;;
;; 参数
;; ----
;; path : string | path?
;; 待读取的文件路径。
;; keepends : boolean (可选，默认 #f)
;; 是否保留每行末尾的换行符。
;; offset : exact-integer? (可选，默认 0)
;; 从第几行开始读取（0-based 索引）。
;; limit : exact-integer? | #f (可选，默认 #f)
;; 最多读取的行数，#f 表示读至文件末尾。
;;
;; 返回值
;; -----
;; list
;; 返回包含各行字符串的列表；文件为空或 offset 超过总行数时返回 '()。
;;
;; 错误处理
;; ----
;; type-error 当 path 非 string 且非 path?、keepends 非 boolean、offset 或 limit 非精确整数（limit 允许 #f）时。
;; value-error 当 offset 为负数、limit 为非正整数（如 0 或 -1）时。
;; file-not-found-error 当文件不存在时。

;; 测试项1：默认按行读取与 keepends 换行保留测试
(let ((test-file (path-join (path-temp-dir) "path-read-lines-basic.txt")))
  (when (path-exists? test-file)
    (delete-file (path->string test-file))
  ) ;when
  (path-write-text test-file "line 1\nline 2\nline 3\n")
  (check (path-read-lines test-file) => '("line 1" "line 2" "line 3"))
  (check (path-read-lines (path->string test-file))
    =>
    '("line 1" "line 2" "line 3")
  ) ;check
  (check (path-read-lines test-file #t) => '("line 1\n" "line 2\n" "line 3\n"))
  (check (path-read-lines test-file #f 1 1) => '("line 2"))
  (check (path-read-lines test-file #t 1 1) => '("line 2\n"))
  (delete-file (path->string test-file))
) ;let

;; 测试项2：offset 与 limit 大文件局部行切片测试
(let* ((big-file (path-join (path-temp-dir) "path-read-lines-big.txt"))
       (p (open-output-file (path->string big-file)))
      ) ;
  (dynamic-wind (lambda () #f)
    (lambda ()
      (let loop
        ((i 0))
        (when (< i 10000)
          (display "line-" p)
          (display i p)
          (newline p)
          (loop (+ i 1))
        ) ;when
      ) ;let
    ) ;lambda
    (lambda () (close-output-port p))
  ) ;dynamic-wind

  ;; 切片读取 1000 开始的 2000 行
  (let ((slice (path-read-lines big-file #f 1000 2000)))
    (check (length slice) => 2000)
    (check (car slice) => "line-1000")
    (check (list-ref slice 1999) => "line-2999")
  ) ;let

  ;; offset 超过文件总行数时返回 '()
  (check (path-read-lines big-file #f 15000 10) => '())
  (check (path-read-lines big-file #f 10000 10) => '())

  ;; limit 为 #f 时从 offset 读到文件末尾
  (let ((tail (path-read-lines big-file #f 9990 #f)))
    (check (length tail) => 10)
    (check (car tail) => "line-9990")
    (check (list-ref tail 9) => "line-9999")
  ) ;let

  ;; limit 大于剩余行数时返回全部剩余行
  (let ((tail2 (path-read-lines big-file #f 9995 50)))
    (check (length tail2) => 5)
    (check (car tail2) => "line-9995")
    (check (list-ref tail2 4) => "line-9999")
  ) ;let

  (delete-file (path->string big-file))
) ;let*

;; 测试项3：边界场景与换行符归一化测试

;; 空文件
(let ((empty-file (path-join (path-temp-dir) "path-read-lines-empty.txt")))
  (when (path-exists? empty-file)
    (delete-file (path->string empty-file))
  ) ;when
  (path-write-text empty-file "")
  (check (path-read-lines empty-file) => '())
  (check (path-read-lines empty-file #t) => '())
  (delete-file (path->string empty-file))
) ;let

;; 末行不带换行符
(let ((no-trail-file (path-join (path-temp-dir) "path-read-lines-notrail.txt")))
  (when (path-exists? no-trail-file)
    (delete-file (path->string no-trail-file))
  ) ;when
  (path-write-text no-trail-file "a\nb")
  (check (path-read-lines no-trail-file) => '("a" "b"))
  (check (path-read-lines no-trail-file #t) => '("a\n" "b"))
  (delete-file (path->string no-trail-file))
) ;let

;; 单行无换行符
(let ((single-file (path-join (path-temp-dir) "path-read-lines-single.txt")))
  (when (path-exists? single-file)
    (delete-file (path->string single-file))
  ) ;when
  (path-write-text single-file "single line")
  (check (path-read-lines single-file) => '("single line"))
  (check (path-read-lines single-file #t) => '("single line"))
  (delete-file (path->string single-file))
) ;let

;; 仅有换行符
(let ((nl-file (path-join (path-temp-dir) "path-read-lines-nl.txt")))
  (when (path-exists? nl-file)
    (delete-file (path->string nl-file))
  ) ;when
  (path-write-text nl-file "\n")
  (check (path-read-lines nl-file) => '(""))
  (check (path-read-lines nl-file #t) => '("\n"))
  (delete-file (path->string nl-file))
) ;let

;; 多行含空行
(let ((multi-empty (path-join (path-temp-dir) "path-read-lines-multiempty.txt")))
  (when (path-exists? multi-empty)
    (delete-file (path->string multi-empty))
  ) ;when
  (path-write-text multi-empty "start\n\n\nend")
  (check (path-read-lines multi-empty) => '("start" "" "" "end"))
  (check (path-read-lines multi-empty #t) => '("start\n" "\n" "\n" "end"))
  (delete-file (path->string multi-empty))
) ;let

;; CRLF 换行符归一化
(let ((crlf-file (path-join (path-temp-dir) "path-read-lines-crlf.txt")))
  (when (path-exists? crlf-file)
    (delete-file (path->string crlf-file))
  ) ;when
  (g_path-write-text (path->string crlf-file) "Hello\r\nWorld\r\n")
  (check (path-read-lines crlf-file) => '("Hello" "World"))
  (check (path-read-lines crlf-file #t) => '("Hello\n" "World\n"))
  (delete-file (path->string crlf-file))
) ;let

;; 孤立 CR 换行符归一化
(let ((cr-file (path-join (path-temp-dir) "path-read-lines-cr.txt")))
  (when (path-exists? cr-file)
    (delete-file (path->string cr-file))
  ) ;when
  (g_path-write-text (path->string cr-file) "Hello\rWorld\r")
  (check (path-read-lines cr-file) => '("Hello" "World"))
  (check (path-read-lines cr-file #t) => '("Hello\n" "World\n"))
  (delete-file (path->string cr-file))
) ;let

;; 孤立 CR 末行不带换行符
(let ((cr-notrail-file (path-join (path-temp-dir) "path-read-lines-cr-notrail.txt")))
  (when (path-exists? cr-notrail-file)
    (delete-file (path->string cr-notrail-file))
  ) ;when
  (g_path-write-text (path->string cr-notrail-file) "Hello\rWorld")
  (check (path-read-lines cr-notrail-file) => '("Hello" "World"))
  (check (path-read-lines cr-notrail-file #t) => '("Hello\n" "World"))
  (delete-file (path->string cr-notrail-file))
) ;let

;; 中文文本
(let ((zh-file (path-join (path-temp-dir) "path-read-lines-zh.txt")))
  (when (path-exists? zh-file)
    (delete-file (path->string zh-file))
  ) ;when
  (path-write-text zh-file
    "你好，世界！\n第二行中文\n第三行中文\n"
  ) ;path-write-text
  (check (path-read-lines zh-file)
    =>
    '("你好，世界！" "第二行中文" "第三行中文")
  ) ;check
  (check (path-read-lines zh-file #t)
    =>
    '("你好，世界！\n" "第二行中文\n" "第三行中文\n")
  ) ;check
  (delete-file (path->string zh-file))
) ;let

;; 测试项4：错误处理与防御性健壮性测试
(let ((dummy-file (path-join (path-temp-dir) "path-read-lines-dummy.txt")))
  (when (path-exists? dummy-file)
    (delete-file (path->string dummy-file))
  ) ;when
  (path-write-text dummy-file "hello\n")

  ;; 文件不存在
  (check-catch 'file-not-found-error
    (path-read-lines (path "/this/file/does/not/exist"))
  ) ;check-catch
  (check-catch 'file-not-found-error
    (path-read-lines "/this/file/does/not/exist")
  ) ;check-catch

  ;; path 类型错误
  (check-catch 'type-error (path-read-lines #\a))
  (check-catch 'type-error (path-read-lines 123))
  (check-catch 'type-error (path-read-lines 'symbol))

  ;; keepends 类型错误
  (check-catch 'type-error (path-read-lines dummy-file "invalid"))
  (check-catch 'type-error (path-read-lines dummy-file 1))

  ;; offset 类型错误
  (check-catch 'type-error (path-read-lines dummy-file #f "0"))
  (check-catch 'type-error (path-read-lines dummy-file #f 1.5))

  ;; limit 类型错误
  (check-catch 'type-error (path-read-lines dummy-file #f 0 "10"))
  (check-catch 'type-error (path-read-lines dummy-file #f 0 2.5))

  ;; offset 取值错误 (value-error)
  (check-catch 'value-error (path-read-lines dummy-file #f -1))
  (check-catch 'value-error (path-read-lines dummy-file #f -100))

  ;; limit 取值错误 (value-error)
  (check-catch 'value-error (path-read-lines dummy-file #f 0 0))
  (check-catch 'value-error (path-read-lines dummy-file #f 0 -1))

  (delete-file (path->string dummy-file))
) ;let

(check-report)

;; pi-execute-read
;; 读取文件内容（可分页）
;;
;; 语法
;; ----
;; (pi-execute-read args)
;;
;; 参数
;; ----
;; args : list?，工具参数
;;
;; 返回值
;; ------
;; record?，工具结果

(import (liii check)
  (liii string)
  (liii path)
  (liii base64)
  (liii pi ai message)
  (liii pi code core tools read)
  (liii pi code core tools)
) ;import

(check-set-mode! 'report-failed)

(define (%read-error-msg thunk)
  (catch 'error
    thunk
    (lambda (tag . info) (if (and (pair? info) (pair? (car info))) (caar info) ""))
  ) ;catch
) ;define

(define (%tmp-file name)
  (path->string (path-join (path-temp-dir) name))
) ;define

(define test-file
  (cond ((file-exists? "tools/pi/tests/fixtures/sample.txt")
         "tools/pi/tests/fixtures/sample.txt"
        ) ;
        ((file-exists? "tests/fixtures/sample.txt") "tests/fixtures/sample.txt")
        ((file-exists? "../../fixtures/sample.txt") "../../fixtures/sample.txt")
        (else "tools/pi/tests/fixtures/sample.txt")
  ) ;cond
) ;define

(let ((content (pi-execute-read `(("path" . ,test-file) ("limit" . 2)))))
  (check-true (string? content))
  (check content
    =>
    "hello sample\nline 2\n\n[1 more line in file. Use offset=3 to continue.]"
  ) ;check
) ;let

(check
  (%read-error-msg
    (lambda () (pi-execute-read `(("path" . ,test-file) ("offset" . 4))))
  ) ;%read-error-msg
  =>
  "Offset 4 is beyond end of file (3 lines total)"
) ;check

;; 测试项1 & 2：标准换行结尾文件的总行数、剩余行数与越界 offset 报告
(let ((sample-file (%tmp-file "pi-sample-1073.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n"))
  ) ;pi-execute-write
  ;; limit=1 读取 2 行文件，提示剩余 1 行
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  ;; 完整读取
  (check (pi-execute-read (list (cons "path" sample-file))) => "line1\nline2")
  ;; offset=2 读取最后 1 行
  (check (pi-execute-read (list (cons "path" sample-file) (cons "offset" 2)))
    =>
    "line2"
  ) ;check
  ;; 越界 offset 报错行数符合实际 (2 lines total)
  (check
    (%read-error-msg
      (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
    ) ;%read-error-msg
    =>
    "Offset 3 is beyond end of file (2 lines total)"
  ) ;check
  (check
    (%read-error-msg
      (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
    ) ;%read-error-msg
    =>
    "Offset 4 is beyond end of file (2 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; 末尾无换行测试
(let ((sample-file (%tmp-file "pi-sample-no-eol.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  (check-catch 'error
    (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3)))
  ) ;check-catch
  (delete-file sample-file)
) ;let

;; 末尾多换行测试 (3 lines total)
(let ((sample-file (%tmp-file "pi-sample-multi-nl.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\nline2\n\n"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 2)))
    =>
    "line1\nline2\n\n[1 more line in file. Use offset=3 to continue.]"
  ) ;check
  (check
    (%read-error-msg
      (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 4))))
    ) ;%read-error-msg
    =>
    "Offset 4 is beyond end of file (3 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; 空文件测试 (0 lines total)
(let ((sample-file (%tmp-file "pi-sample-empty.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "")))
  (check (pi-execute-read (list (cons "path" sample-file))) => "")
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    ""
  ) ;check
  (check
    (%read-error-msg
      (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 1))))
    ) ;%read-error-msg
    =>
    "Offset 1 is beyond end of file (0 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; CRLF 换行格式测试
(let ((sample-file (%tmp-file "pi-sample-crlf.txt")))
  (pi-execute-write (list (cons "path" sample-file) (cons "content" "line1\r\nline2\r\n"))
  ) ;pi-execute-write
  (check (pi-execute-read (list (cons "path" sample-file) (cons "limit" 1)))
    =>
    "line1\n\n[1 more line in file. Use offset=2 to continue.]"
  ) ;check
  (check
    (%read-error-msg
      (lambda () (pi-execute-read (list (cons "path" sample-file) (cons "offset" 3))))
    ) ;%read-error-msg
    =>
    "Offset 3 is beyond end of file (2 lines total)"
  ) ;check
  (delete-file sample-file)
) ;let

;; 读取源码文件、offset/limit 与错误路径

(define legacy-test-file
  (if (file-exists? "liii/pi/code/cli.scm")
    "liii/pi/code/cli.scm"
    "tools/pi/liii/pi/code/cli.scm"
  ) ;if
) ;define

(let ((res (pi-execute-read (list (cons "path" legacy-test-file)))))
  (check-true (string? res))
  (check-true (string-contains? res "(define-library (liii pi code cli)"))
) ;let

(let ((res (pi-execute-read (list (cons "path" legacy-test-file) (cons "limit" 2)))))
  (check-true (string-contains? res "more lines in file. Use offset=3 to continue.")
  ) ;check-true
) ;let

(let ((res (pi-execute-read (list (cons "path" legacy-test-file) (cons "offset" 3) (cons "limit" 2))
           ) ;pi-execute-read
      ) ;res
     ) ;
  (check-true (string-contains? res "more lines in file. Use offset=5 to continue.")
  ) ;check-true
) ;let

;; 浮点形式的 offset/limit：模型偶尔用 1.0/90.0 这类浮点表示行号，
;; 数值上为整数的应按整数处理，不能泄露 list-drop 内部类型错误
(let ((content (pi-execute-read (list (cons "path" test-file) (cons "offset" 1.0) (cons "limit" 2.0))
               ) ;pi-execute-read
      ) ;content
     ) ;
  (check-true (string? content))
  (check content
    =>
    "hello sample\nline 2\n\n[1 more line in file. Use offset=3 to continue.]"
  ) ;check
) ;let

;; 浮点 offset 的越界报错信息使用整数形式
(check
  (%read-error-msg
    (lambda () (pi-execute-read (list (cons "path" test-file) (cons "offset" 4.0))))
  ) ;%read-error-msg
  =>
  "Offset 4 is beyond end of file (3 lines total)"
) ;check

;; 非整数值的浮点参数：报清晰的参数错误，而不是 list-drop 的内部错误
(check-catch 'error
  (pi-execute-read (list (cons "path" test-file) (cons "offset" 1.5)))
) ;check-catch
(check-catch 'error
  (pi-execute-read (list (cons "path" test-file) (cons "limit" 2.5)))
) ;check-catch

(check-catch 'error
  (pi-execute-read (list (cons "path" legacy-test-file) (cons "offset" 999999)))
) ;check-catch
(check-catch 'error
  (pi-execute-read '(("path" . "non_existent_file_12345.txt")))
) ;check-catch
(check-catch 'error (pi-execute-read '(("path" . "tests"))))
(check-catch 'error
  (pi-execute-read (list (cons "path" legacy-test-file)) :timed-out? #t)
) ;check-catch

;; 图片读取测试：PNG, JPEG, GIF, WebP, BMP 与 Base64 编码保真
(let* ((png-path (%tmp-file "pi-test-img.png"))
       (png-bytes #u8(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82))
      ) ;
  (path-write-bytes (path png-path) png-bytes)
  (let ((res (pi-execute-read (list (cons "path" png-path)))))
    (check-true (pi-tool-result? res))
    (check-false (pi-tool-result-is-error res))
    (let* ((content (vector->list (pi-tool-result-content res)))
           (text-blk (car content))
           (img-blk (cadr content))
          ) ;
      (check (length content) => 2)
      (check-true (pi-text-block? text-blk))
      (check (pi-text-block-text text-blk) => "Read image file [image/png]")
      (check-true (pi-image-block? img-blk))
      (check (pi-image-block-mime-type img-blk) => "image/png")
      (check (bytevector-base64-decode (string->utf8 (pi-image-block-data img-blk)))
        =>
        png-bytes
      ) ;check
    ) ;let*
  ) ;let
  (delete-file png-path)
) ;let*

;; JPEG 文件读取
(let* ((jpg-path (%tmp-file "pi-test-img.jpg"))
       (jpg-bytes #u8(255 216 255 224 0 16 74 70 73 70))
      ) ;
  (path-write-bytes (path jpg-path) jpg-bytes)
  (let* ((res (pi-execute-read (list (cons "path" jpg-path))))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
         (img-blk (cadr content))
        ) ;
    (check (pi-text-block-text text-blk) => "Read image file [image/jpeg]")
    (check (pi-image-block-mime-type img-blk) => "image/jpeg")
    (check (bytevector-base64-decode (string->utf8 (pi-image-block-data img-blk)))
      =>
      jpg-bytes
    ) ;check
  ) ;let*
  (delete-file jpg-path)
) ;let*

;; GIF 文件读取
(let* ((gif-path (%tmp-file "pi-test-img.gif"))
       (gif-bytes #u8(71 73 70 56 57 97 1 0 1 0))
      ) ;
  (path-write-bytes (path gif-path) gif-bytes)
  (let* ((res (pi-execute-read (list (cons "path" gif-path))))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
         (img-blk (cadr content))
        ) ;
    (check (pi-text-block-text text-blk) => "Read image file [image/gif]")
    (check (pi-image-block-mime-type img-blk) => "image/gif")
  ) ;let*
  (delete-file gif-path)
) ;let*

;; WebP 文件读取
(let* ((webp-path (%tmp-file "pi-test-img.webp"))
       (webp-bytes #u8(82 73 70 70 0 0 0 0 87 69 66 80 0 0 0 0))
      ) ;
  (path-write-bytes (path webp-path) webp-bytes)
  (let* ((res (pi-execute-read (list (cons "path" webp-path))))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
         (img-blk (cadr content))
        ) ;
    (check (pi-text-block-text text-blk) => "Read image file [image/webp]")
    (check (pi-image-block-mime-type img-blk) => "image/webp")
  ) ;let*
  (delete-file webp-path)
) ;let*

;; BMP 文件读取
(let* ((bmp-path (%tmp-file "pi-test-img.bmp"))
       (bmp-bytes #u8(66 77 0 0 0 0 0 0 0 0))
      ) ;
  (path-write-bytes (path bmp-path) bmp-bytes)
  (let* ((res (pi-execute-read (list (cons "path" bmp-path))))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
         (img-blk (cadr content))
        ) ;
    (check (pi-text-block-text text-blk) => "Read image file [image/bmp]")
    (check (pi-image-block-mime-type img-blk) => "image/bmp")
  ) ;let*
  (delete-file bmp-path)
) ;let*

;; 魔数未命中但扩展名为 .png 的回退检测
(let* ((ext-path (%tmp-file "pi-fallback.png")) (ext-bytes #u8(1 2 3 4 5)))
  (path-write-bytes (path ext-path) ext-bytes)
  (let* ((res (pi-execute-read (list (cons "path" ext-path))))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
         (img-blk (cadr content))
        ) ;
    (check (pi-text-block-text text-blk) => "Read image file [image/png]")
    (check (pi-image-block-mime-type img-blk) => "image/png")
  ) ;let*
  (delete-file ext-path)
) ;let*

;; 不支持视觉的模型：文本块追加降级提示
(let* ((png-path (%tmp-file "pi-test-non-vision.png"))
       (png-bytes #u8(137 80 78 71 13 10 26 10 0 0 0 0))
       (non-vision-model '((name . "text-model") (input . #("text"))))
      ) ;
  (path-write-bytes (path png-path) png-bytes)
  (let* ((res (pi-execute-read (list (cons "path" png-path)) :model non-vision-model))
         (content (vector->list (pi-tool-result-content res)))
         (text-blk (car content))
        ) ;
    (check-true (string-contains? (pi-text-block-text text-blk)
                  "[Current model does not support images. The image will be omitted from this request.]"
                ) ;string-contains?
    ) ;check-true
  ) ;let*
  (delete-file png-path)
) ;let*

(check-report)

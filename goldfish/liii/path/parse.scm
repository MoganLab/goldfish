    ;; ; 字符串按 sep 切分为 vector(保留空段)。Windows/posix 通用。
    (define (string-split-vec str sep)
      (list->vector (string-split str sep))
    ) ;define

    ;; ; 判断字符串是否以 UNC 前缀(\\)开头。仅 Windows 路径用,posix 永远 #f。
    (define (unc-prefix? s)
      (and (>= (string-length s) 2)
        (char=? (string-ref s 0) #\\)
        (char=? (string-ref s 1) #\\)
      ) ;and
    ) ;define

    ;; ; 判断字符串是否为带盘符的 Windows 路径(如 "C:" 开头)。posix 永远 #f。
    (define (windows-path-with-drive? s)
      (and (>= (string-length s) 2)
        (char-alphabetic? (string-ref s 0))
        (char=? (string-ref s 1) #\:)
      ) ;and
    ) ;define

    ;; ; 提取 Windows 路径字符串的盘符字母(大写,不含冒号)。仅 Windows 路径用。
    (define (extract-drive s)
      (string (ascii-upcase (string-ref s 0)))
    ) ;define

    ;; ; 过滤掉 "." 段和空段。pathlib 风格:构造时丢弃 "." 与连续/尾分隔符
    ;; ; 产生的空段,保留 ".." 直到 resolve() 才处理。使 /tmp/ → /tmp、
    ;; ; a//b → a/b 与 pathlib 一致。
    (define (drop-dot-parts v)
      (vector-filter (lambda (p) (not (or (string=? p ".") (string-null? p)))) v)
    ) ;define

    ;; ; 路径字符串解析为 (values parts root),适用于 posix 与 Windows
    ;; ; 普通路径(无 UNC、无盘符)。
    ;; ; 差异:Windows 平台下先把 / 规范化为 \,再按当前平台 sep 切分;
    ;; ; posix 平台直接按 / 切分。起始分隔符由 root 字段表达,不混入 parts。
    (define (parse-path-string s)
      (cond ((string-null? s) (cons #(".") #f))
            ((string=? s ".") (cons #(".") #f))
            ((string=? s "/") (cons #() #\/))
            ((string=? s "\\") (cons #() #\\))
            (else (let ((sep (os-sep)))
                    (let ((normalized (if (os-windows?) (string-replace s "/" "\\") s)))
                      (if (and (> (string-length normalized) 0) (char=? (string-ref normalized 0) sep))
                        ;; 绝对路径:丢弃 string-split-vec 产生的起始空 stub。
                        (let ((raw (string-split-vec normalized sep)))
                          (cons (drop-dot-parts (vector-drop raw 1)) sep)
                        ) ;let
                        ;; 相对路径
                        (cons (drop-dot-parts (string-split-vec normalized sep)) #f)
                      ) ;if
                    ) ;let
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 解析 UNC 路径 \\server[\share][\path...] 为 (values parts drive root)。
    ;; ; 仅 Windows 用。差异:仅 \\server(无 share)时 root=#f(对齐 pathlib);
    ;; ; 含 share 时 root=#\\。
    (define (parse-unc normalized len)
      (let* ((after-slash (substring normalized 2 len))
             (first-slash (string-index after-slash #\\))
            ) ;
        (if (not first-slash)
          ;; 仅 \\server（无 share）：drive 是 \\\\server 整体，但 root 为 #f
          ;; (对齐 pathlib: PureWindowsPath('\\\\srv').root == '')
          (list #() (string-append "\\\\" after-slash) #f)
          (let* ((server (substring after-slash 0 first-slash))
                 (rest (substring after-slash (+ first-slash 1) (string-length after-slash)))
                 (second-slash (string-index rest #\\))
                ) ;
            (if (not second-slash)
              ;; \\server\share（无后续路径）
              (list #() (string-append "\\\\" server "\\" rest) #\\)
              ;; \\server\share\path...
              (let* ((share (substring rest 0 second-slash))
                     (path-rest (substring rest (+ second-slash 1) (string-length rest)))
                     (parts (if (string-null? path-rest)
                              #()
                              (drop-dot-parts (string-split-vec path-rest #\\))
                            ) ;if
                     ) ;parts
                    ) ;
                (list parts (string-append "\\\\" server "\\" share) #\\)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; ; Windows 路径字符串解析为 (values parts drive root)。仅 Windows 用。
    ;; ; 同时识别 \ 和 / 作为分隔符(Windows API 两种都接受)。涵盖四种形式:
    ;; ;   \\server\share\a\b → drive="\\server\share" root=#\\ parts=#("a" "b")
    ;; ;   C:\a\b             → drive="C"              root=#\\ parts=#("a" "b")
    ;; ;   C:foo              → drive="C"              root=#f  parts=#("foo")
    ;; ;   \foo               → drive=""               root=#\\ parts=#("foo")
    ;; ;   foo\bar            → drive=""               root=#f  parts=#("foo" "bar")
    (define (parse-windows-path s)
      (let* ((normalized (string-replace s "/" "\\")) (len (string-length normalized)))
        (cond
          ;; UNC 路径: \\server\share[\path...]
          ((unc-prefix? normalized) (parse-unc normalized len))

          ;; 盘符绝对路径: C:\...
          ((and (>= len 3)
             (char-alphabetic? (string-ref normalized 0))
             (char=? (string-ref normalized 1) #\:)
             (char=? (string-ref normalized 2) #\\)
           ) ;and
           (let* ((drive (extract-drive normalized))
                  (rest (substring normalized 3 len))
                  (parts (if (string-null? rest) #() (drop-dot-parts (string-split-vec rest #\\)))
                  ) ;parts
                 ) ;
             (list parts drive #\\)
           ) ;let*
          ) ;

          ;; 盘符相对路径: C:foo
          ((windows-path-with-drive? normalized)
           (let* ((drive (extract-drive normalized))
                  (rest (substring normalized 2 len))
                  (parts (if (string-null? rest) #() (drop-dot-parts (string-split-vec rest #\\)))
                  ) ;parts
                 ) ;
             (list parts drive #f)
           ) ;let*
          ) ;

          ;; 当前盘根路径: \foo
          ((and (> len 0) (char=? (string-ref normalized 0) #\\))
           (let* ((rest (substring normalized 1 len))
                  (parts (if (string-null? rest) #() (drop-dot-parts (string-split-vec rest #\\)))
                  ) ;parts
                 ) ;
             (list parts "" #\\)
           ) ;let*
          ) ;

          ;; 相对路径: foo\bar
          (else (list (drop-dot-parts (string-split-vec normalized #\\)) "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 构造路径 anchor 首元素字符串(对齐 pathlib.parts 的首元素)。
    ;; ; 差异:posix 绝对返回 "/";Windows 返回 "C:\"/"C:"/"\\server\share\"/"\\";
    ;; ; 纯相对路径(无 drive 无 root)返回 #f。
    (define (anchor-string type drive root)
      (cond ((eq? type 'posix) (and root "/"))
            ((unc-prefix? drive) (string-append drive "\\"))
            ((not (string-null? drive)) (string-append drive ":" (if root "\\" "")))
            (root "\\")
            (else #f)
      ) ;cond
    ) ;define

    ;; ; 将纯净 parts 段用 sep 拼接成字符串(不含 drive/anchor/stub)。
    ;; ; 调用方根据路径类型传 "/"(posix)或 "\\"(Windows)。
    (define (parts->string parts sep)
      (string-join (vector->list parts) sep)
    ) ;define

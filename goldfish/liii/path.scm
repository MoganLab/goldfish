(define-library (liii path)
  (export path
    path?
    path-copy
    path-copy-into
    path-dir?
    path-file?
    path-exists?
    path-getsize
    path-read-text
    path-read-bytes
    path-write-text
    path-write-bytes
    path-append-text
    path-touch
    path-root
    path-of-drive
    path-from-parts
    path-from-env
    path-cwd
    path-home
    path-temp-dir
    path-parts
    path-type
    path-drive
    path->string
    path-from-string
    path-name
    path-stem
    path-suffix
    path-equals?
    path=?
    path-absolute?
    path-relative?
    path-join
    path-parent
    path-list
    path-list-path
    path-rmdir
    path-unlink
    path-rename
  ) ;export
  (import (liii base)
    (liii error)
    (liii os)
    (liii string)
    (liii vector)
    (scheme base)
    (scheme char)
    (liii ascii)
  ) ;import
  (begin

    ;; ; Path record 类型
    ;; ; root 字段：#\\ 表示有根分隔符（绝对路径或驱动器根），#f 表示无根（drive-relative 或相对路径）
    ;; ; 用于区分 C:\foo（root=#\\）和 C:foo（root=#f）的语义差异
    (define-record-type <path>
      (make-path-record parts type drive root)
      path?
      (parts path-record-parts path-record-set-parts!)
      (type path-record-type path-record-set-type!)
      (drive path-record-drive path-record-set-drive!)
      (root path-record-root path-record-set-root!)
    ) ;define-record-type

    ;; ; 将字符串按字符 sep 切分为 vector。封装 (liii string) 的 string-split,保留空段。
    (define (string-split-vec str sep)
      (list->vector (string-split str sep))
    ) ;define

    ;; ; 将路径字符串解析为 parts 与 root 标志。
    ;; ; 返回 (values parts root),其中 root 是起始分隔符字符或 #f。
    ;; ; parts 是纯净的段向量;起始的 "/" (Windows 上为 "\\") 由 root 字段表达,
    ;; ; 不再以空字符串 stub 的形式混入 parts。
    (define (parse-path-string s)
      (cond ((string-null? s) (values #(".") #f))
            ((string=? s ".") (values #(".") #f))
            ((string=? s "/") (values #() #\/))
            ((string=? s "\\") (values #() #\\))
            (else (let ((sep (os-sep)))
                    (let ((normalized (if (os-windows?) (string-replace s "/" "\\") s)))
                      (if (and (> (string-length normalized) 0) (char=? (string-ref normalized 0) sep))
                        ;; 绝对路径:丢弃 string-split-vec 产生的起始空 stub。
                        (let ((raw (string-split-vec normalized sep)))
                          (values (drop-dot-parts (vector-drop raw 1)) sep)
                        ) ;let
                        ;; 相对路径
                        (values (drop-dot-parts (string-split-vec normalized sep)) #f)
                      ) ;if
                    ) ;let
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 判断字符串是否为带盘符的 Windows 路径(如 "C:" 开头)
    (define (windows-path-with-drive? s)
      (and (>= (string-length s) 2)
        (char-alphabetic? (string-ref s 0))
        (char=? (string-ref s 1) #\:)
      ) ;and
    ) ;define

    ;; ; 判断字符串是否以 UNC 前缀(\\ 或 //)开头
    (define (unc-prefix? s)
      (and (>= (string-length s) 2)
        (char=? (string-ref s 0) #\\)
        (char=? (string-ref s 1) #\\)
      ) ;and
    ) ;define

    ;; ; 提取 Windows 路径字符串的盘符字母(不含冒号)
    (define (extract-drive s)
      (string (ascii-upcase (string-ref s 0)))
    ) ;define

    ;; ; 过滤掉纯 "." 段(pathlib 风格:构造时丢弃 ".",但保留 ".." 直到 resolve() 才处理)。
    (define (drop-dot-parts v)
      (vector-filter (lambda (p) (not (string=? p "."))) v)
    ) ;define

    ;; ; 解析 Windows 路径字符串为 parts 与 root 标志。
    ;; ; 返回 (values parts drive root),其中:
    ;; ;   parts: 纯净段向量(不含 drive、anchor、分隔符 stub)
    ;; ;   drive: "C" 或 "" (无冒号);UNC 路径下存的是 "\\server\share" anchor
    ;; ;   root:  路径有锚(UNC share / C:\ / 当前盘根 \foo)时为 #\\,否则为 #f
    ;; ; 同时识别 \ 和 / 作为分隔符:Windows API 接受两种,但 s7 的 port-filename
    ;; ; 和命令行参数经常返回正斜杠路径(如 "C:/Users/.../foo.scm"),不做规范化会
    ;; ; 导致 string-split-vec 一刀不切、path-parent 把整串当单一 part。
    ;;
    ;; ; 涵盖四种 Windows 路径形式（对齐 pathlib.PureWindowsPath）:
    ;; ;   \\server\share\a\b  → drive="\\server\share"  root=#\\  parts=#("a" "b")
    ;; ;   C:\a\b              → drive="C"               root=#\\  parts=#("a" "b")
    ;; ;   C:foo               → drive="C"               root=#f   parts=#("foo")     drive-relative
    ;; ;   \foo                → drive=""                root=#\\  parts=#("foo")     current-drive root
    ;; ;   foo\bar             → drive=""                root=#f   parts=#("foo" "bar")
    ;; ; parse-unc: 解析 \\server\share[\path...] 形式的 UNC 路径。
    ;; ; 返回 (values parts drive root)。
    (define (parse-unc normalized len)
      (let* ((after-slash (substring normalized 2 len))
             (first-slash (string-index after-slash #\\))
            ) ;
        (if (not first-slash)
          ;; 仅 \\server（无 share），将整段当作 anchor。
          (values #() (string-append "\\\\" after-slash) #\\)
          (let* ((server (substring after-slash 0 first-slash))
                 (rest (substring after-slash (+ first-slash 1) (string-length after-slash)))
                 (second-slash (string-index rest #\\))
                ) ;
            (if (not second-slash)
              ;; \\server\share（无后续路径）
              (values #() (string-append "\\\\" server "\\" rest) #\\)
              ;; \\server\share\path...
              (let* ((share (substring rest 0 second-slash))
                     (path-rest (substring rest (+ second-slash 1) (string-length rest)))
                     (parts (if (string-null? path-rest)
                              #()
                              (drop-dot-parts (string-split-vec path-rest #\\))
                            ) ;if
                     ) ;parts
                    ) ;
                (values parts (string-append "\\\\" server "\\" share) #\\)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (parse-windows-path s)
      (let* ((normalized (string-replace s "/" "\\")) (len (string-length normalized)))
        (cond
          ;; UNC: \\server\share[\path...]
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
             (values parts drive #\\)
           ) ;let*
          ) ;

          ;; 盘符相对路径: C:foo
          ((windows-path-with-drive? normalized)
           (let* ((drive (extract-drive normalized))
                  (rest (substring normalized 2 len))
                  (parts (if (string-null? rest) #() (drop-dot-parts (string-split-vec rest #\\)))
                  ) ;parts
                 ) ;
             (values parts drive #f)
           ) ;let*
          ) ;

          ;; 当前盘根路径: \foo
          ((and (> len 0) (char=? (string-ref normalized 0) #\\))
           (let* ((rest (substring normalized 1 len))
                  (parts (if (string-null? rest) #() (drop-dot-parts (string-split-vec rest #\\)))
                  ) ;parts
                 ) ;
             (values parts "" #\\)
           ) ;let*
          ) ;

          ;; 相对路径: foo\bar
          (else (values (drop-dot-parts (string-split-vec normalized #\\)) "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 构造 path 对象。
    ;; ; Windows 路径形式（UNC、C:\、C:foo）只在 Windows 平台识别；
    ;; ; 其他平台一律当 posix 处理,避免污染非 Windows 调用方。
    (define (path . args)
      (if (null? args)
        (make-path-record #(".") 'posix "" #f)
        (let ((arg (car args)))
          (cond ((string? arg)
                 (if (and (os-windows?) (or (unc-prefix? arg) (windows-path-with-drive? arg)))
                   (receive (parts drive root)
                     (parse-windows-path arg)
                     (make-path-record parts 'windows drive root)
                   ) ;receive
                   (receive (parts root)
                     (parse-path-string arg)
                     (let ((type (if (os-windows?) 'windows 'posix)))
                       (make-path-record parts type "" root)
                     ) ;let
                   ) ;receive
                 ) ;if
                ) ;
                ((path? arg) (copy arg))
                (else (type-error "path: argument must be string or path"))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (path-copy source target)
      (let ((src (path->string source)) (dst (path->string target)))
        (if (not (file-exists? src))
          (file-not-found-error (string-append "No such file or directory: '" src "'"))
          (g_path-copy src dst)
        ) ;if
      ) ;let
    ) ;define

    (define (path-copy-into source target-dir)
      (let ((filename (path-name (path-from-string source))))
        (path-copy source
          (path->string (path-join (path-from-string target-dir) (path filename)))
        ) ;path-copy
      ) ;let
    ) ;define

    ;; ; 获取 parts 向量
    ;; ; 对外契约:absolute 路径在 parts 前保留 stub,用于与历史 caller round-trip。
    ;; ;   posix 绝对(root=#\/, drive=""): 加 "/" stub
    ;; ;   UNC / drive 路径: drive 字段已表达绝对性,parts 不加 stub
    (define (path-parts p)
      (if (not (path? p))
        (type-error "path-parts: argument must be path")
        (let ((parts (path-record-parts p))
              (root (path-record-root p))
              (drive (path-record-drive p))
              (type (path-record-type p))
             ) ;
          (cond ((not root) (vector-copy parts))
                ((and (eq? type 'posix) (string-null? drive)) (vector-append #("/") parts))
                (else (vector-copy parts))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; ; 获取 type ('posix 或 'windows)
    (define (path-type p)
      (if (path? p)
        (path-record-type p)
        (type-error "path-type: argument must be path")
      ) ;if
    ) ;define

    ;; ; 获取盘符字母(仅 Windows 路径)
    (define (path-drive p)
      (if (path? p)
        (path-record-drive p)
        (type-error "path-drive: argument must be path")
      ) ;if
    ) ;define

    ;; ; 将 path 转换为字符串。
    ;; ; 拼装规则:
    ;; ;   posix:  root=#\/ 时加 "/" 前缀(parts 是纯段),否则按相对路径拼接。
    ;; ;   windows: 三类 —— UNC(drive 形如 \\srv\sh)、drive-absolute(C:\)、drive-relative(C:foo),
    ;; ;           以及无 drive 的 current-drive root(\foo) 与相对(foo\bar)。
    (define (path->string p)
      (cond ((path? p)
             (let ((parts (path-record-parts p))
                   (type (path-record-type p))
                   (drive (path-record-drive p))
                   (root (path-record-root p))
                  ) ;
               (case type
                ((posix)
                 (let ((body (parts->string parts "/")))
                   (cond ((and root (not (string-null? body))) (string-append "/" body))
                         (root "/")
                         ((string-null? body) ".")
                         (else body)
                   ) ;cond
                 ) ;let
                ) ;
                ((windows)
                 (let ((body (parts->string parts "\\")))
                   (cond
                     ;; UNC: drive 字段已含 \\server\share anchor
                     ((unc-prefix? drive)
                      (if (string-null? body) drive (string-append drive "\\" body))
                     ) ;
                     ;; drive-absolute 或 drive-relative: drive 是单个盘符如 "C"
                     ((not (string-null? drive))
                      (let ((prefix (string-append drive ":" (if root "\\" ""))))
                        (if (string-null? body) prefix (string-append prefix body))
                      ) ;let
                     ) ;
                     ;; 无 drive: root=#\\ 为当前盘根 "\foo",否则为相对 "foo\bar"
                     (root (if (string-null? body) "\\" (string-append "\\" body)))
                     (else (if (string-null? body) "." body))
                   ) ;cond
                 ) ;let
                ) ;
                (else (value-error "path->string: unknown type"))
               ) ;case
             ) ;let
            ) ;
            ((string? p) p)
            (else (type-error "path->string: argument must be path or string"))
      ) ;cond
    ) ;define

    (define (path-from-string s)
      (path s)
    ) ;define

    ;; ; 将纯净的 parts 段用 sep 拼接成字符串。
    ;; ; parts 不含驱动器、anchor 或绝对性 stub;绝对性由调用方根据 root 字段自行处理。
    (define (parts->string parts sep)
      (string-join (vector->list parts) sep)
    ) ;define

    ;; ; Check if two paths are equal
    (define (path-equals? p1 p2)
      (let ((s1 (path->string (path p1))) (s2 (path->string (path p2))))
        (string=? s1 s2)
      ) ;let
    ) ;define

    (define path=? path-equals?)

    ;; ; 判断路径是否为绝对路径。
    ;; ; Windows: 同时有 drive 和 root（C:\foo 或 \\srv\sh\foo）才算绝对;
    ;; ;          C:foo（drive-relative, root=#f）不算绝对。
    ;; ; POSIX:  root=#\/ 即为绝对。
    ;; ; 传入字符串时,先转成 path 再判断,保证语义与 path 版本一致。
    (define (path-absolute? p)
      (if (path? p)
        (let ((type (path-record-type p))
              (drive (path-record-drive p))
              (root (path-record-root p))
             ) ;
          (case type
           ((windows) (and root (not (string-null? drive))))
           ((posix) (if root #t #f))
           (else #f)
          ) ;case
        ) ;let
        (path-absolute? (path p))
      ) ;if
    ) ;define

    ;; ; Check if path is relative
    (define (path-relative? p)
      (not (path-absolute? p))
    ) ;define

    ;; ; 获取路径的末段(文件名)
    (define (path-name p)
      (let ((s (path->string p)))
        ;; 空串与 "." 都表示当前目录,文件名留空。
        (if (or (string-null? s) (string=? s "."))
          ""
          (let* ((sep (os-sep)) (idx (string-index-right s sep)))
            (if idx (substring s (+ idx 1) (string-length s)) s)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; ; 获取文件名去掉扩展名的部分(stem)
    (define (path-stem p)
      (let ((name (path-name p)))
        (let ((splits (string-split name #\.)))
          (let ((count (length splits)))
            (cond ((<= count 1) name)
                  ((string=? name ".") "")
                  ((string=? name "..") "..")
                  ;; 形如 ".bashrc":首段为空且仅两段,整体作为 stem
                  ((and (string=? (car splits) "") (= count 2)) name)
                  ;; 去掉最后一段,其余用 "." 连接
                  (else (string-join (reverse (cdr (reverse splits))) "."))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;; ; Get the suffix (file extension)
    (define (path-suffix p)
      (let ((name (path-name p)))
        (let ((splits (string-split name #\.)))
          (let ((count (length splits)))
            (cond ((<= count 1) "")
                  ((string=? name ".") "")
                  ((string=? name "..") "")
                  ((and (string=? (car splits) "") (= count 2)) "")
                  (else (string-append "." (list-ref splits (- count 1))))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;; ; 拼接多个路径段。
    ;; ; 已带末尾分隔符的中间结果不再补 sep,避免出现双斜杠。
    (define (path-join base . segments)
      (let ((sep (string (os-sep))))
        (define (join-one acc seg)
          (if (or (string-null? acc) (string-ends? acc sep))
            (string-append acc seg)
            (string-append acc sep seg)
          ) ;if
        ) ;define
        (let loop
          ((acc (path->string base)) (rest segments))
          (if (null? rest) acc (loop (join-one acc (path->string (car rest))) (cdr rest)))
        ) ;let
      ) ;let
    ) ;define

    ;; ; 获取父目录。
    ;; ; Windows 上同时识别 / 和 \: port-filename / argv 经常返回正斜杠路径。
    (define (path-parent p)
      (let* ((raw (path->string p))
             (sep (os-sep))
             ;; 归一化分隔符:Windows 上把 / 换成 \,使 string-index-right 能命中
             (s (if (os-windows?) (string-replace raw "/" "\\") raw))
             ;; 末尾分隔符去掉(根路径除外),避免 path-parent "/a/" 找到第二个 /
             (s-len (string-length s))
             (s-trimmed (if (and (> s-len 1) (char=? (string-ref s (- s-len 1)) sep))
                          (substring s 0 (- s-len 1))
                          s
                        ) ;if
             ) ;s-trimmed
             (trim-len (string-length s-trimmed))
             (idx (string-index-right s-trimmed sep))
            ) ;
        (cond ((not idx) (if (os-windows?) (path "") (path ".")))
              ((= idx 0) (path-root))
              ;; 保留末尾分隔符作为父目录表示(与历史行为一致)
              (else (path (substring s-trimmed 0 (+ idx 1))))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; Path predicates and operations (work with strings or paths)
    (define (path-dir? p)
      (g_isdir (path->string p))
    ) ;define

    (define (path-file? p)
      (g_isfile (path->string p))
    ) ;define

    (define (path-exists? p)
      (file-exists? (path->string p))
    ) ;define

    (define (path-getsize p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error (string-append "No such file or directory: '" s "'"))
          (g_path-getsize s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-read-text p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error (string-append "No such file or directory: '" s "'"))
          (g_path-read-text s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-read-bytes p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error (string-append "No such file or directory: '" s "'"))
          (g_path-read-bytes s)
        ) ;if
      ) ;let
    ) ;define

    (define (path-write-text p content)
      (if (not (string? content))
        (type-error "path-write-text: content must be string")
        (g_path-write-text (path->string p) content)
      ) ;if
    ) ;define

    (define (path-write-bytes p data)
      (if (not (byte-vector? data))
        (type-error "path-write-bytes: data must be bytevector")
        (g_path-write-bytes (path->string p) data)
      ) ;if
    ) ;define

    (define (path-append-text p content)
      (g_path-append-text (path->string p) content)
    ) ;define

    (define (path-touch p)
      (g_path-touch (path->string p))
    ) ;define

    ;; ; Static path constructors
    ;; ; path-root: posix 根 "/", root=#\\ 表示有根分隔符
    (define (path-root)
      (make-path-record #() 'posix "" #\/)
    ) ;define

    ;; ; path-of-drive: 驱动器根 "C:\",drive="C", root=#\\, parts=#()
    (define (path-of-drive ch)
      (if (char? ch)
        (make-path-record #() 'windows (string (ascii-upcase ch)) #\\)
        (type-error "path-of-drive: argument must be char")
      ) ;if
    ) ;define

    ;; ; path-from-parts: 从 parts vector 构造路径。
    ;; ; 识别首段:
    ;; ;   "C:" / "c:"  → drive-absolute（windows）, root=#\\
    ;; ;   "/"          → posix 绝对, root=#\/
    ;; ;   "\\"         → windows 当前驱动器根, root=#\\
    ;; ; 其他视为相对路径的纯 parts。
    (define (path-from-parts parts)
      (if (not (vector? parts))
        (type-error "path-from-parts: argument must be vector")
        (if (= (vector-length parts) 0)
          (make-path-record #(".") 'posix "" #f)
          (let ((head (vector-ref parts 0)))
            (cond ((and (string? head) (windows-path-with-drive? head))
                   (let ((drive (extract-drive head)))
                     (let ((clean-parts (let loop
                                          ((i 1) (result '()))
                                          (if (>= i (vector-length parts))
                                            (list->vector (reverse result))
                                            (let ((part (vector-ref parts i)))
                                              (if (or (string-null? part) (string=? part "/") (string=? part "\\"))
                                                (loop (+ i 1) result)
                                                (loop (+ i 1) (cons part result))
                                              ) ;if
                                            ) ;let
                                          ) ;if
                                        ) ;let
                           ) ;clean-parts
                          ) ;
                       ;; 调用方传 "C:" 默认视为 drive-absolute（root=#\\），符合 path-of-drive 文档
                       (make-path-record clean-parts 'windows drive #\\)
                     ) ;let
                   ) ;let
                  ) ;

                  ((string=? head "/")
                   ;; posix 绝对：保留 caller 传的剩余 parts（不再剥离 "."，因为 caller 显式传）
                   (make-path-record (vector-copy (vector-drop parts 1)) 'posix "" #\/)
                  ) ;

                  ((string=? head "\\")
                   ;; windows 当前驱动器根
                   (make-path-record (vector-copy (vector-drop parts 1)) 'windows "" #\\)
                  ) ;

                  ((string-null? head)
                   ;; 老式空 stub 开头，按 posix 绝对处理
                   (make-path-record (vector-copy (vector-drop parts 1)) 'posix "" #\/)
                  ) ;

                  (else (make-path-record (vector-copy parts) 'posix "" #f))
            ) ;cond
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    (define (path-from-env name)
      (path (getenv name))
    ) ;define

    (define (path-cwd)
      (path (getcwd))
    ) ;define

    (define (path-home)
      (cond ((or (os-linux?) (os-macos?)) (path (getenv "HOME")))
            ((os-windows?) (path (string-append (getenv "HOMEDRIVE") (getenv "HOMEPATH"))))
            (else (value-error "path-home: unknown platform"))
      ) ;cond
    ) ;define

    (define (path-temp-dir)
      (path (os-temp-dir))
    ) ;define

    ;; ; List directory contents
    (define (path-list p)
      (listdir (path->string p))
    ) ;define

    ;; ; List directory contents as path objects
    (define (path-list-path p)
      (let ((base (path->string p)))
        (let ((entries (listdir base)))
          (vector-map (lambda (entry) (path-join base entry)) entries)
        ) ;let
      ) ;let
    ) ;define

    ;; ; Remove directory
    (define (path-rmdir p)
      (rmdir (path->string p))
    ) ;define

    ;; ; Remove file
    (define* (path-unlink p (missing-ok #f))
      (let ((s (path->string p)))
        (cond ((file-exists? s) (remove s))
              (missing-ok #t)
              (else (error 'file-not-found-error (string-append "File not found: " s)))
        ) ;cond
      ) ;let
    ) ;define*

    ;; ; Rename file or directory
    (define (path-rename src dst)
      (rename (path->string src) (path->string dst))
    ) ;define

  ) ;begin
) ;define-library

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
    path-suffixes
    path-with-name
    path-with-stem
    path-with-suffix
    path-relative-to
    path-equals?
    path=?
    path-absolute?
    path-relative?
    path-join
    path-parent
    path-parents
    path-list
    path-list-path
    path-rmdir
    path-unlink
    path-rename
    path-mkdir
    path-absolute
    path-expanduser
    path-match
    path-as-posix
    path-resolve
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

    ;; ; 过滤掉 "." 段和空段(pathlib 风格:构造时丢弃 "." 与连续/尾分隔符产生的空段,
    ;; ; 保留 ".." 直到 resolve() 才处理)。使 /tmp/ → /tmp、a//b → a/b 与 pathlib 一致。
    (define (drop-dot-parts v)
      (vector-filter (lambda (p) (not (or (string=? p ".") (string-null? p)))) v)
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
           ((posix) (and root #t))
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

    ;; ; 解析末段的点分隔结构,返回 (values stem-list suffix-list)。
    ;; ;   stem-list   : 作为 stem 的点段列表(无前导 ".")
    ;; ;   suffix-list : 每个后缀含前导 "." 的列表
    ;; ; 规则(对齐 pathlib):
    ;; ;   "." / ".."        → stem=name, 无后缀
    ;; ;   首段为空(.bashrc) → 整体作 stem, 无后缀(隐藏文件不切)
    ;; ;   单段(name 无点)   → stem=name, 无后缀
    ;; ;   其他(a.b.c)       → stem=a.b, suffixes=(.c)…(此处按末段切)
    (define (split-name-dots name)
      (cond ((or (string=? name ".") (string=? name "..")) (values (list name) '()))
            (else (let ((splits (string-split name #\.)))
                    (if (or (<= (length splits) 1) (string=? (car splits) ""))
                      (values (list name) '())
                      ;; splits 不含前导空:stem 段 = 除末段外, suffix = 末段
                      (let* ((rev (reverse splits)) (suffix-seg (car rev)) (stem-segs (reverse (cdr rev))))
                        (values stem-segs (list (string-append "." suffix-seg)))
                      ) ;let*
                    ) ;if
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 获取文件名去掉扩展名的部分(stem)
    (define (path-stem p)
      (let-values (((stem-segs _) (split-name-dots (path-name p))))
        (string-join stem-segs ".")
      ) ;let-values
    ) ;define

    ;; ; Get the suffix (file extension)
    (define (path-suffix p)
      (let-values (((_ suffix-segs) (split-name-dots (path-name p))))
        (if (null? suffix-segs) "" (car suffix-segs))
      ) ;let-values
    ) ;define

    ;; ; 获取末段的所有后缀向量(对齐 pathlib .suffixes)。
    ;; ; 多个点分段时,从首个点起的每段都算一个后缀(含前导 ".")。
    ;; ; 隐藏文件(.bashrc)、无点文件、"."/".." 均返回 #()。
    (define (path-suffixes p)
      (let ((name (path-name p)))
        (cond ((or (string=? name ".") (string=? name "..")) #())
              (else (let ((splits (string-split name #\.)))
                      (if (or (<= (length splits) 1) (string=? (car splits) ""))
                        #()
                        (list->vector (map (lambda (s) (string-append "." s)) (cdr splits)))
                      ) ;if
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 返回末段替换为 new-name 后的新 path(保留 drive/root/其他段)。
    ;; ; 对空路径(parts 为空或仅 ".")直接返回单段 new-name 的相对路径。
    (define (replace-last-segment p new-name)
      (let* ((pp (path p))
             (parts (path-record-parts pp))
             (n (vector-length parts))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
             (root (path-record-root pp))
            ) ;
        (cond ((or (= n 0) (and (= n 1) (string=? (vector-ref parts 0) ".")))
               (make-path-record (vector new-name) 'posix "" #f)
              ) ;
              (else (let ((new-parts (vector-copy parts)))
                      (vector-set! new-parts (- n 1) new-name)
                      (make-path-record new-parts type drive root)
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 替换末段整体名称(对齐 pathlib.with_name)。
    (define (path-with-name p new-name)
      (if (not (string? new-name))
        (type-error "path-with-name: new-name must be string")
        (replace-last-segment p new-name)
      ) ;if
    ) ;define

    ;; ; 替换末段的 stem,保留原所有后缀(对齐 pathlib.with_stem)。
    (define (path-with-stem p new-stem)
      (if (not (string? new-stem))
        (type-error "path-with-stem: new-stem must be string")
        (let ((suffixes (path-suffixes p)))
          ;; 无后缀(含隐藏文件):替换整段,等价于 with-name
          (if (= (vector-length suffixes) 0)
            (replace-last-segment p new-stem)
            (replace-last-segment p
              (apply string-append (cons new-stem (vector->list suffixes)))
            ) ;replace-last-segment
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; ; 替换末段后缀(对齐 pathlib.with_suffix)。
    ;; ; ext 为 "" 去后缀;含前导 "." 时替换/追加。隐藏文件追加新后缀。
    (define (path-with-suffix p ext)
      (if (not (string? ext))
        (type-error "path-with-suffix: ext must be string")
        (let* ((name (path-name p)) (stem (path-stem p)))
          (cond ((string-null? ext)
                 (if (string-null? (path-suffix (path p)))
                   (path p)
                   (replace-last-segment p stem)
                 ) ;if
                ) ;
                ((not (char=? (string-ref ext 0) #\.))
                 (value-error "path-with-suffix: ext must start with '.'")
                ) ;
                (else (replace-last-segment p (string-append stem ext)))
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    ;; ; 计算 p 相对于 base 的相对路径(对齐 pathlib.relative_to)。
    ;; ; 要求两者 anchor 一致(drive、root、type 都相同),且 base 的纯段是 p 的前缀;
    ;; ; 否则报错。base 段等于 p 段时返回 "."。
    (define (path-relative-to p base)
      (let* ((pp (path p))
             (bp (path base))
             (p-segs (path-record-parts pp))
             (b-segs (path-record-parts bp))
             (p-drive (path-record-drive pp))
             (b-drive (path-record-drive bp))
             (p-root (path-record-root pp))
             (b-root (path-record-root bp))
             (pn (vector-length p-segs))
             (bn (vector-length b-segs))
            ) ;
        (define (same-anchor?)
          (and (string=? p-drive b-drive)
            (eq? p-root b-root)
            (eq? (path-record-type pp) (path-record-type bp))
          ) ;and
        ) ;define
        (define (prefix-match?)
          ;; 检查 b-segs 是 p-segs 的前缀
          (let loop
            ((i 0))
            (cond ((= i bn) #t)
                  ((not (string=? (vector-ref p-segs i) (vector-ref b-segs i))) #f)
                  (else (loop (+ i 1)))
            ) ;cond
          ) ;let
        ) ;define
        (define (remaining-segments)
          ;; 取 p-segs 从 bn 起的剩余段
          (let* ((rlen (- pn bn)) (rest (make-vector rlen)))
            (let fill
              ((j 0))
              (if (= j rlen)
                rest
                (begin
                  (vector-set! rest j (vector-ref p-segs (+ bn j)))
                  (fill (+ j 1))
                ) ;begin
              ) ;if
            ) ;let
          ) ;let*
        ) ;define
        (cond ((not (same-anchor?))
               (value-error "path-relative-to: paths do not share an anchor")
              ) ;
              ((> bn pn) (value-error "path-relative-to: path is not relative to base"))
              ((not (prefix-match?))
               (value-error "path-relative-to: path is not relative to base")
              ) ;
              ((= bn pn) (path "."))
              (else (make-path-record (remaining-segments) 'posix "" #f))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 拼接多个路径段(对齐 pathlib.joinpath)。
    ;; ; 遇到绝对路径参数(root 或 drive+root)时,丢弃已有累加结果,从该参数重置
    ;; ; (pathlib: PurePath("/a").joinpath("/b") => "/b")。
    ;; ; 相对参数则追加到当前 acc,并避免出现双斜杠。
    (define (path-join base . segments)
      (let ((sep (string (os-sep))))
        (define (join-one acc seg-str)
          (if (or (string-null? acc) (string-ends? acc sep))
            (string-append acc seg-str)
            (string-append acc sep seg-str)
          ) ;if
        ) ;define
        (let loop
          ((acc (path->string base)) (rest segments))
          (if (null? rest)
            acc
            (let* ((seg-path (path (car rest))) (seg-str (path->string seg-path)))
              (if (path-absolute? seg-path)
                (loop seg-str (cdr rest))
                (loop (join-one acc seg-str) (cdr rest))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ; 获取父目录(对齐 pathlib.parent,不含末尾分隔符)。
    ;; ;   /tmp/demo.txt → /tmp   /a → /   / → /   a/b → a   a → .   "" → .
    ;; ;   C:\Users → C:\   \\srv\sh\a → \\srv\sh
    ;; ; 基于 record 的 parts 实现:去掉末段后保留 drive/root 重建,
    ;; ; 避免字符串扫描在 drive/UNC anchor 上的边界错误。
    (define (path-parent p)
      (let* ((pp (path p))
             (parts (path-record-parts pp))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
             (root (path-record-root pp))
             (n (vector-length parts))
            ) ;
        (cond
          ;; 无段:根路径(/、C:\、\\srv\sh)或空相对路径,parent 为自身
          ((= n 0) pp)
          ;; 仅一段:取决于是否有 anchor。
          ;;  - 有 root(绝对 /a、C:\Users、\foo):parent 为其 anchor(/、C:\、\)
          ;;  - 无 root(相对 a、foo):parent 为当前目录 "."
          ((= n 1)
           (if root
             (cond ((and (eq? type 'posix) (string-null? drive)) (path-root))
                   ((not (string-null? drive)) (path-of-drive (string-ref drive 0)))
                   ;; 无 drive 但有 root:windows current-drive root (\foo → \)
                   (else (make-path-record #() 'windows "" root))
             ) ;cond
             (path ".")
           ) ;if
          ) ;
          ;; 多段:去掉末段,保留 drive/root
          (else (make-path-record (vector-drop-right parts 1) type drive root))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 获取所有祖先路径向量(对齐 pathlib .parents),从最近父路径到最远。
    ;; ; 绝对路径终止于根/anchor;相对路径不补 "."(pathlib 对 'a' 的 parents 为空)。
    (define (path-parents p)
      (let ((start (path p)))
        (let loop
          ((cur start) (acc '()))
          (let* ((par (path-parent cur))
                 (par-str (path->string par))
                 (cur-str (path->string cur))
                ) ;
            (cond
              ;; 相对路径回溯到当前目录:停止,不纳入 "."
              ((string=? par-str ".") (list->vector (reverse acc)))
              ;; parent 等于自身(cur 已是根/anchor):停止(cur 已在上一轮纳入,或 cur 是 start)
              ((string=? par-str cur-str) (list->vector (reverse acc)))
              (else (loop par (cons par acc)))
            ) ;cond
          ) ;let*
        ) ;let
      ) ;let
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
                     ;; 跳过首段,滤掉空段与分隔符 stub(复用 drop-dot-parts 同款规则,用 vector-filter)
                     (let ((clean-parts (vector-filter (lambda (part)
                                                         (not (or (string-null? part) (string=? part "/") (string=? part "\\")))
                                                       ) ;lambda
                                          (vector-drop parts 1)
                                        ) ;vector-filter
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

    ;; ; 用户主目录(对齐 pathlib.Path.home 的环境变量解析顺序)。
    ;; ; POSIX: HOME;Windows: USERPROFILE,缺失则回退 HOMEDRIVE+HOMEPATH。
    ;; ; 环境变量缺失时显式报错(与 pathlib 一致,不静默回退到错误路径)。
    (define (path-home)
      (cond ((or (os-linux?) (os-macos?))
             (let ((h (getenv "HOME")))
               (if h (path h) (value-error "path-home: HOME is not set"))
             ) ;let
            ) ;
            ((os-windows?)
             (let ((profile (getenv "USERPROFILE")))
               (cond (profile (path profile))
                     ((and (getenv "HOMEDRIVE") (getenv "HOMEPATH"))
                      (path (string-append (getenv "HOMEDRIVE") (getenv "HOMEPATH")))
                     ) ;
                     (else (value-error "path-home: USERPROFILE and HOMEDRIVE/HOMEPATH are not set"))
               ) ;cond
             ) ;let
            ) ;
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

    ;; ; 返回绝对路径(对齐 pathlib.Path.absolute):相对路径拼 cwd,绝对路径保持。
    ;; ; 不解析符号链接(区别于 path-resolve)。返回 path 对象。
    (define (path-absolute p)
      (let ((pp (path p)))
        (if (path-absolute? pp) pp (path (path-join (path-cwd) pp)))
      ) ;let
    ) ;define

    ;; ; 展开 ~ 为用户主目录(对齐 pathlib.Path.expanduser 的 ~ 部分;不支持 ~user)。
    ;; ; ~/x → home/x。~ 后的剩余串作为相对段拼接(用字符串拼接,避免触发 path-join
    ;; ; 的绝对参数重置)。
    (define (path-expanduser p)
      (let* ((pp (path p)) (s (path->string pp)))
        (if (string-starts? s "~")
          (let* ((rest (substring s 1 (string-length s))) (home-str (path->string (path-home))))
            (path (cond ((string-null? rest) home-str)
                        ((string-starts? rest "/") (string-append home-str rest))
                        (else (string-append home-str "/" rest))
                  ) ;cond
            ) ;path
          ) ;let*
          pp
        ) ;if
      ) ;let*
    ) ;define

    ;; ; 返回用 / 作分隔符的字符串(对齐 pathlib.PurePath.as_posix)。
    ;; ; posix 下等价于 path->string;windows 下把 \ 转成 /。
    (define (path-as-posix p)
      (let ((s (path->string p)))
        (if (os-windows?) (string-replace s "\\" "/") s)
      ) ;let
    ) ;define

    ;; ; 字符类 [..] 匹配单字符 ch。pattern[j] 是 '['。
    ;; ; 返回 (values matched? next-index-after-])。
    (define (charclass-match-one pattern plen j ch)
      (let ((negate (and (< (+ j 1) plen) (char=? (string-ref pattern (+ j 1)) #\^))))
        (let scan
          ((k (if negate (+ j 2) (+ j 1))) (matched #f))
          (cond ((or (>= k plen) (char=? (string-ref pattern k) #\]))
                 (values (if negate (not matched) matched) (if (< k plen) (+ k 1) k))
                ) ;
                ((and (< (+ k 2) plen) (char=? (string-ref pattern (+ k 1)) #\-))
                 (let ((lo (string-ref pattern k)) (hi (string-ref pattern (+ k 2))))
                   (let ((hit (if (char<=? lo hi)
                                (and (char>=? ch lo) (char<=? ch hi))
                                (and (char>=? ch hi) (char<=? ch lo))
                              ) ;if
                         ) ;hit
                        ) ;
                     (scan (+ k 3) (or matched hit))
                   ) ;let
                 ) ;let
                ) ;
                (else (scan (+ k 1) (or matched (char=? (string-ref pattern k) ch))))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; ; glob 单层匹配(支持 * / ? / [seq],不跨分隔符)。递归,无副作用。
    ;; ;   *  匹配任意个字符   ?  匹配单个字符   [c1c2]/[^..]/[a-z] 字符集
    (define (glob-match? pattern str)
      (let ((plen (string-length pattern)) (slen (string-length str)))
        (letrec ((match-at (lambda (p0 s0)
                             (cond ((= p0 plen) (= s0 slen))
                                   ((char=? (string-ref pattern p0) #\*)
                                    ;; * 尝试匹配 0..(slen-s0) 个字符
                                    (let try-star
                                      ((n 0))
                                      (cond ((match-at (+ p0 1) (+ s0 n)) #t)
                                            ((< (+ s0 n) slen) (try-star (+ n 1)))
                                            (else #f)
                                      ) ;cond
                                    ) ;let
                                   ) ;
                                   ((= s0 slen) #f)
                                   ((char=? (string-ref pattern p0) #\?) (match-at (+ p0 1) (+ s0 1)))
                                   ((char=? (string-ref pattern p0) #\[)
                                    (let-values (((hit next) (charclass-match-one pattern plen p0 (string-ref str s0))))
                                      (and hit (match-at next (+ s0 1)))
                                    ) ;let-values
                                   ) ;
                                   ((char=? (string-ref pattern p0) (string-ref str s0))
                                    (match-at (+ p0 1) (+ s0 1))
                                   ) ;
                                   (else #f)
                             ) ;cond
                           ) ;lambda
                 ) ;match-at
                ) ;
          (match-at 0 0)
        ) ;letrec
      ) ;let
    ) ;define

    ;; ; glob 模式匹配路径末段(对齐 pathlib.PurePath.match 的末段语义)。
    (define (path-match p pattern)
      (glob-match? pattern (path-name p))
    ) ;define

    ;; ; 规范化绝对路径:消除 . 段、折叠 .. 段(对齐 pathlib.Path.resolve 的纯规范化部分)。
    ;; ; 注:因无 realpath 原语,不解析符号链接(与 pathlib strict 语义有差异)。
    (define (normalize-absolute p)
      (let* ((pp (path-absolute p))
             (segs (vector->list (path-record-parts pp)))
             (root (path-record-root pp))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
            ) ;
        (let loop
          ((rest segs) (acc '()))
          (cond ((null? rest) (make-path-record (list->vector (reverse acc)) type drive root))
                ((string=? (car rest) "..")
                 (if (null? acc)
                   ;; .. 在根之上:丢弃(不能越过根)
                   (loop (cdr rest) acc)
                   (loop (cdr rest) (cdr acc))
                 ) ;if
                ) ;
                (else (loop (cdr rest) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    ;; ; 解析为规范化的绝对路径(对齐 pathlib.Path.resolve 的简化版)。
    ;; ; 绝对化 + 折叠 . / .. 段;不解析符号链接(无 realpath 原语)。
    (define (path-resolve p)
      (normalize-absolute p)
    ) ;define

    ;; ; 创建目录(对齐 pathlib.Path.mkdir)。
    ;; ; parents=#t 递归创建中间目录;exist_ok=#t 时已存在不报错。
    (define* (path-mkdir p (parents #f) (exist-ok #f))
      (define (ensure-one s)
        (if (file-exists? s) #t (mkdir s))
      ) ;define
      (let ((s (path->string p)))
        (cond ((and (not exist-ok) (file-exists? s))
               (file-exists-error (string-append "File exists: '" s "'"))
              ) ;
              ((not parents) (ensure-one s))
              (else
                ;; parents:从根向下逐级创建(跳过已存在的)
                (let loop
                  ((rest (reverse (vector->list (path-parents (path s))))))
                  (if (null? rest)
                    (ensure-one s)
                    (begin
                      (ensure-one (path->string (car rest)))
                      (loop (cdr rest))
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library

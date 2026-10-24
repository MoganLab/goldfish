;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii path record)
  (export make-path-record path? path-record-parts path-record-set-parts!
    path-record-type path-record-set-type! path-record-drive
    path-record-set-drive! path-record-root path-record-set-root! anchor-string
    parts->string path path-from-string path-of-drive path-from-parts
    path-from-env path-cwd path-home path-temp-dir path-parts path-type
    path-drive path-root path->string path-as-posix path-absolute?
    path-relative? path-equals? path=?
  ) ;export
  (import (scheme base)
    (scheme char)
    (liii ascii)
    (liii base)
    (liii error)
    (liii os)
    (liii string)
    (liii vector)
    (liii path parse)
  ) ;import
  (begin

    ;; ;============================================================
    ;; ; Path record 类型
    ;; ;============================================================
    ;; ; root 字段：#\\ / #\/ 表示有根分隔符(绝对路径或驱动器根),
    ;; ; #f 表示无根(drive-relative 或相对路径)。用于区分 C:\foo
    ;; ; (root=#\\)和 C:foo(root=#f)的语义差异。
    (define-record-type <path>
      (make-path-record parts type drive root)
      path?
      (parts path-record-parts path-record-set-parts!)
      (type path-record-type path-record-set-type!)
      (drive path-record-drive path-record-set-drive!)
      (root path-record-root path-record-set-root!)
    ) ;define-record-type

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

    ;; ;============================================================
    ;; ; 公共构造与基础属性函数
    ;; ;============================================================

    ;; ; 对齐 pathlib.PurePath(*pathsegment)
    ;; ; 差异:Windows 路径形式(UNC、C:\、C:foo)只在 Windows 平台识别,
    ;; ; 否则一律当 posix;同一字符串 "/" 在 Windows 被规范化为 windows "\"。
    (define (path . args)
      (cond ((null? args) (make-path-record #(".") 'posix "" #f))
            (else
              (let ((arg (car args)))
                (cond
                 ((string? arg)
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
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 对齐 pathlib.PurePath.parts
    ;; ; 差异:绝对路径首元素是 anchor — posix "/", Windows "C:\"/"\\srv\sh\\"/"\\";
    ;; ; drive-relative(C:foo)首元素 "C:";相对路径无 anchor。
    (define (path-parts p)
      (if (not (path? p))
        (type-error "path-parts: argument must be path")
        (let ((parts (path-record-parts p))
              (root (path-record-root p))
              (drive (path-record-drive p))
              (type (path-record-type p))
             ) ;
          (let ((anchor (anchor-string type drive root)))
            (cond (anchor (vector-append (vector anchor) parts))
                  ((and (= (vector-length parts) 1) (string=? (vector-ref parts 0) ".")) #())
                  (else (vector-copy parts))
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; ; 对齐 pathlib.PurePath.parser 风格 — 返回 'posix 或 'windows。
    ;; ; 差异:类型由构造时的平台决定,Windows 上 (path "/") 是 'windows。
    (define (path-type p)
      (if (path? p)
        (path-record-type p)
        (type-error "path-type: argument must be path")
      ) ;if
    ) ;define

    ;; ; 对齐 pathlib.PurePath.drive
    ;; ; 差异:posix 恒为 "";Windows drive-absolute/drive-relative 返回 "C:"(带冒号),
    ;; ; UNC 返回 "\\server\share" 整体(无冒号),相对/current-drive root 返回 ""。
    (define (path-drive p)
      (if (path? p)
        (let* ((drive (path-record-drive p)) (type (path-record-type p)))
          (cond ((eq? type 'posix) "")
                ((unc-prefix? drive) drive)
                ((string-null? drive) "")
                (else (string-append drive ":"))
          ) ;cond
        ) ;let*
        (type-error "path-drive: argument must be path")
      ) ;if
    ) ;define

    ;; ; 对齐 pathlib.PurePath.root
    ;; ; 差异:有根时 posix 返回 "/",Windows 返回 "\";无根(相对、drive-relative、
    ;; ; 仅 server 的 \\srv)返回 ""。
    (define (path-root p)
      (if (path? p)
        (let ((root (path-record-root p)) (type (path-record-type p)))
          (cond ((not root) "")
                ((eq? type 'windows) "\\")
                (else "/")
          ) ;cond
        ) ;let
        (type-error "path-root: argument must be path")
      ) ;if
    ) ;define

    ;; ; 对齐 str(pathlib.PurePath)
    ;; ; 差异:posix 用 "/" 拼接,Windows 用 "\";Windows UNC share anchor
    ;; ; 在只剩 anchor 时带尾斜杠(对齐 pathlib: str('\\srv\sh')=='\\srv\sh\')。
    (define (%path->string-posix parts root)
      (let ((body (parts->string parts "/")))
        (cond
         ((and root (not (string-null? body))) (string-append "/" body))
         (root "/")
         ((string-null? body) ".")
         (else body)
        ) ;cond
      ) ;let
    ) ;define

    (define (%path->string-windows parts drive root)
      (let ((body (parts->string parts "\\")))
        (cond
          ;; UNC: drive 字段含 \\server[\share] anchor。
          ;; ; 完整 share anchor(\\server\share)只剩 anchor 时带尾斜杠;
          ;; ; 光秃 server(\\srv)不带尾斜杠(root 为空)。
          ((unc-prefix? drive)
           (if (string-null? body)
             (if (string-index (substring drive 2 (string-length drive)) #\\)
               (string-append drive "\\")
               drive
             ) ;if
             (string-append drive "\\" body)
           ) ;if
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
    ) ;define

    (define (path->string p)
      (cond
       ((path? p)
        (let ((parts (path-record-parts p))
              (type (path-record-type p))
              (drive (path-record-drive p))
              (root (path-record-root p))
             ) ;
          (case type
           ((posix) (%path->string-posix parts root))
           ((windows) (%path->string-windows parts drive root))
           (else (value-error "path->string: unknown type"))
          ) ;case
        ) ;let
       ) ;
       ((string? p) p)
       (else (type-error "path->string: argument must be path or string"))
      ) ;cond
    ) ;define

    ;; ; 对齐 pathlib.PurePath(s)
    (define (path-from-string s)
      (path s)
    ) ;define

    ;; ; 对应 pathlib 的 == 操作符
    ;; ; 差异:按 path->string 字符串比对,Windows 上 "C:\a" 与 "C:/a" 不等
    ;; ; (一个反斜杠、一个正斜杠);posix 上 "/" 与 "\" 不等。
    (define (path-equals? p1 p2)
      (let ((s1 (path->string (path p1))) (s2 (path->string (path p2))))
        (string=? s1 s2)
      ) ;let
    ) ;define

    (define path=? path-equals?)

    ;; ; 对齐 pathlib.PurePath.is_absolute()
    ;; ; 差异:posix 仅看 root 字段;Windows 要求 drive 非空且 root 非空
    ;; ; (C:\foo、\\srv\sh\foo 绝对;C:foo、\foo 都不是绝对)。
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

    ;; ; 对应 path-absolute? 的取反。差异:同 path-absolute? 的 Windows/posix 判定。
    (define (path-relative? p)
      (not (path-absolute? p))
    ) ;define

    ;; ; goldfish 特有 — Windows 驱动器根 "C:\" 的便捷构造器。
    ;; ; drive="C", root=#\\, parts=#()。仅 Windows 路径用,posix 无对应。
    (define (path-of-drive ch)
      (if (char? ch)
        (make-path-record #() 'windows (string (ascii-upcase ch)) #\\)
        (type-error "path-of-drive: argument must be char")
      ) ;if
    ) ;define

    ;; ; goldfish 特有 — 从 parts vector 构造路径(与 path-parts 互为逆运算)。
    ;; ; 差异:识别首段 anchor(对齐 pathlib.parts 首元素形式):
    ;; ;   "C:\"   → drive-absolute(windows), root=#\\
    ;; ;   "C:"    → drive-relative(windows),  root=#f
    ;; ;   "\\server\share\" → UNC share anchor, drive="\\server\share", root=#\\
    ;; ;   "\\"    → windows 当前驱动器根, root=#\\
    ;; ;   "/"     → posix 绝对, root=#\/
    (define (path-from-parts parts)
      (cond ((not (vector? parts)) (type-error "path-from-parts: argument must be vector"))
            ((= (vector-length parts) 0) (make-path-record #(".") 'posix "" #f))
            (else
              (let ((head (vector-ref parts 0)))
                (cond
                 ((and (string? head) (unc-prefix? head))
                  ;; UNC share anchor: \\server\share\[...] → drive="\\server\share", root=#\\
                  (let* ((hend (string-length head))
                         (has-tail-slash? (char=? (string-ref head (- hend 1)) #\\))
                         (trimmed (if has-tail-slash? (substring head 0 (- hend 1)) head))
                        ) ;
                    (make-path-record (clean-tail parts) 'windows trimmed #\\)
                  ) ;let*
                 ) ;

                 ((and (string? head) (windows-path-with-drive? head))
                  (let* ((drive (extract-drive head))
                         (hend (string-length head))
                         (has-slash? (and (> hend 2) (char=? (string-ref head 2) #\\)))
                         (root (if has-slash? #\\ #f))
                        ) ;
                    (make-path-record (clean-tail parts) 'windows drive root)
                  ) ;let*
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
            ) ;else
      ) ;cond
    ) ;define

    ;; ; goldfish 特有 — 从环境变量构造路径(等价于 (path (getenv name)))。
    (define (path-from-env name)
      (path (getenv name))
    ) ;define

    ;; ; 当前工作目录(pathlib.Path.cwd() 的模块函数版本)。
    (define (path-cwd)
      (path (getcwd))
    ) ;define

    ;; ; 对齐 pathlib.Path.home 的环境变量解析顺序。
    ;; ; 差异:POSIX 查 HOME;Windows 查 USERPROFILE,缺失则回退 HOMEDRIVE+HOMEPATH。
    (define (path-home)
      (cond
       ((or (os-linux?) (os-macos?))
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

    ;; ; 系统临时目录(POSIX 通常 /tmp,Windows 通常 %TEMP%)。
    (define (path-temp-dir)
      (path (os-temp-dir))
    ) ;define

    ;; ; 对齐 pathlib.PurePath.as_posix()
    ;; ; 差异:posix 下等价于 path->string;Windows 下把 \ 转成 /。
    (define (path-as-posix p)
      (let ((s (path->string p)))
        (if (os-windows?) (string-replace s "\\" "/") s)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

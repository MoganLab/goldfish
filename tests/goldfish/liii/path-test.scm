;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii path)
        (liii check)
        (liii os)
        (liii base)
        (liii string)
) ;import

(check-set-mode! 'report-failed)

(define (string-list-contains? target xs)
  (cond ((null? xs) #f)
        ((string=? target (car xs)) #t)
        (else (string-list-contains? target (cdr xs)))
  ) ;cond
) ;define

(define (path-vector->string-list xs)
  (vector->list (vector-map path->string xs))
) ;define

(define (temp-path name)
  (path-join (path-temp-dir) name)
) ;define

(define (delete-file-if-exists p)
  (when (file-exists? (path->string p))
    (delete-file (path->string p))
  ) ;when
) ;define

(define (delete-dir-if-exists p)
  (when (file-exists? (path->string p))
    (rmdir (path->string p))
  ) ;when
) ;define

#|
path / path-from-string / path-root / path-of-drive / path-from-parts
构造函数与路径值转换。

语法
----
(path [value]) → path-value
(path-from-string string-path) → path-value
(path-root) → path-value
(path-of-drive ch) → path-value
(path-from-parts vector-of-parts) → path-value

返回值
-----
path-value
返回可被其他 `path-*` 函数继续处理的路径值。

描述
----
`(liii path)` 提供与 `(liii rich-path)` 等价的路径能力，但以函数式 API 暴露。
这些构造函数负责创建路径值，后续通过 `path->string`、`path-name`、`path-parent`、
`path-exists?` 等函数进行访问和操作。
|#

;; 基本构造与转换测试
(check (path->string (path)) => ".")
(check (path->string (path "")) => ".")
(check (path->string (path-from-string "archive.tar.gz")) => "archive.tar.gz")
(check (path->string (path-root)) => "/")
(check (path->string (path-of-drive #\c)) => "C:\\")
(when (not (os-windows?))
  (check (path->string (path (path "tmp/demo.txt"))) => "tmp/demo.txt")
  (check (path->string (path-copy (path "tmp/demo.txt"))) => "tmp/demo.txt")
  (check (path-to-string (path "tmp/demo.txt")) => "tmp/demo.txt")
) ;when

(when (os-windows?)
  (check (path->string (path (path "tmp/demo.txt"))) => "tmp\\demo.txt")
  (check (path->string (path-copy (path "tmp/demo.txt"))) => "tmp\\demo.txt")
  (check (path-to-string (path "tmp/demo.txt")) => "tmp\\demo.txt")
) ;when
(check-catch 'type-error (path-of-drive 1))

;; 元信息测试
(check (path-type (path)) => 'posix)
(check (path-type (path-root)) => 'posix)
(check (path-type (path-of-drive #\c)) => 'windows)
(check (path-drive (path-root)) => "")
(check (path-drive (path-of-drive #\c)) => "C")
(check (path-parts (path)) => #("."))
(check (path-parts (path-root)) => #("/"))
(check (path-parts (path-of-drive #\c)) => #())
(check-true (path-equals? (path-copy (path-of-drive #\d)) (path-of-drive #\D)))
(check-true (path-equals? (path "tmp/demo.txt") "tmp/demo.txt"))
(check-true (path=? (path "tmp/demo.txt") (path-copy (path "tmp/demo.txt"))))

;; 平台相关构造测试
(check (path->string (path-cwd)) => (getcwd))
(check (path->string (path-temp-dir)) => (os-temp-dir))
(check-true (path-absolute? (path-cwd)))
(check-true (path-absolute? (path-temp-dir)))

(when (not (os-windows?))
  (check (path->string (path-home)) => (getenv "HOME"))
  (check (path->string (path-from-env "HOME")) => (getenv "HOME"))
  (check (path->string (path-from-parts #("/" "tmp" "demo.txt"))) => "/tmp/demo.txt")
  (check (path-parts (path-from-parts #("/" "tmp" "demo.txt"))) => #("/" "tmp" "demo.txt"))
) ;when

(when (os-windows?)
  (check-true (path-absolute? (path-home)))
  (check-true (path-exists? (path-home)))
  (check (path->string (path-from-env "USERPROFILE")) => (getenv "USERPROFILE"))
  (check (path->string (path-from-parts #("C:" "tmp" "demo.txt"))) => "C:\\tmp\\demo.txt")
) ;when

#|
path-name / path-stem / path-suffix
获取文件名、主名与扩展名。

语法
----
(path-name path-value) → string
(path-stem path-value) → string
(path-suffix path-value) → string

描述
----
这些函数是 `rich-path` 中 `:name`、`:stem`、`:suffix` 的函数式对应版本，
适用于普通文件名、隐藏文件、多后缀文件以及绝对/相对路径。
|#

;; path-name 测试
(check (path-name (path "file.txt")) => "file.txt")
(check (path-name (path "archive.tar.gz")) => "archive.tar.gz")
(check (path-name (path ".hidden")) => ".hidden")
(check (path-name (path "noext")) => "noext")
(check (path-name (path "")) => "")
(check (path-name (path ".")) => "")
(check (path-name (path "..")) => "..")
(when (not (os-windows?))
  (check (path-name (path "/path/to/file.txt")) => "file.txt")
) ;when
(check (path-name (path-join (path-of-drive #\C) "Users" "report.txt")) => "report.txt")

;; path-stem 测试
(check (path-stem (path "file.txt")) => "file")
(check (path-stem (path "archive.tar.gz")) => "archive.tar")
(check (path-stem (path ".hidden")) => ".hidden")
(check (path-stem (path "noext")) => "noext")
(check (path-stem (path "")) => "")
(check (path-stem (path ".")) => "")
(check (path-stem (path "..")) => "..")
(check (path-stem (path "config.yaml.bak")) => "config.yaml")
(check (path-stem (path "test-file.name-with-dots.txt")) => "test-file.name-with-dots")

;; path-suffix 测试
(check (path-suffix (path "file.txt")) => ".txt")
(check (path-suffix (path "archive.tar.gz")) => ".gz")
(check (path-suffix (path ".hidden")) => "")
(check (path-suffix (path "noext")) => "")
(check (path-suffix (path "")) => "")
(check (path-suffix (path ".")) => "")
(check (path-suffix (path "..")) => "")
(check (path-suffix (path-join (path-of-drive #\C) "Users" "report.txt")) => ".txt")

#|
path-absolute? / path-relative?
判断路径是否为绝对路径或相对路径。

语法
----
(path-absolute? path-value) → boolean
(path-relative? path-value) → boolean

描述
----
这两个函数分别对应 `rich-path` 中 `:absolute?` 与 `:relative`。
它们只根据路径值自身的结构判断，不要求路径实际存在。
|#

;; 基本绝对/相对路径测试
(check-false (path-absolute? (path)))
(check-true (path-relative? (path)))
(check-false (path-absolute? (path "")))
(check-true (path-relative? (path "relative.txt")))
(check-false (path-absolute? (path "relative.txt")))
(check-true (path-absolute? (path-of-drive #\C)))
(check-false (path-relative? (path-of-drive #\C)))

(when (not (os-windows?))
  (check-true (path-absolute? (path-root)))
  (check-true (path-absolute? (path-join (path-root) "tmp")))
  (check-true (path-absolute? (path "/tmp/demo.txt")))
  (check-false (path-relative? (path "/tmp/demo.txt")))
) ;when

(check-true (path-absolute? (path-home)))
(check-true (path-absolute? (path-temp-dir)))

#|
path-join / path-parent
进行路径拼接与父路径获取。

语法
----
(path-join base seg1 seg2 ...) → path-value
(path-parent path-value) → path-value

描述
----
`path-join` 是 `rich-path` 中 `:/` 的函数式版本，
`path-parent` 是 `:parent` 的函数式版本。
|#

(let ((sep (string (os-sep))))
  ;; path-join 测试
  (check (path->string (path-join (path "tmp") "demo.txt"))
         => (string-append "tmp" sep "demo.txt"))
  (check (path->string (path-join (path "tmp") "a" "b" "c.txt"))
         => (string-append "tmp" sep "a" sep "b" sep "c.txt"))

  (when (not (os-windows?))
    (check (path->string (path-join (path-root) "tmp" "demo.txt")) => "/tmp/demo.txt")
    (check-true (path-equals? (path-join (path-root) (path "tmp/demo.txt"))
                              (path "/tmp/demo.txt")))
  ) ;when

  ;; path-parent 测试
  (check (path->string (path-parent (path "tmp/demo.txt")))
         => (string-append "tmp" sep))
  (check (path->string (path-parent (path "tmp"))) => ".")
  (check (path->string (path-parent (path ""))) => ".")
  (when (not (os-windows?))
    (check (path->string (path-parent (path-root))) => "/")
    (check (path->string (path-parent (path "/tmp/"))) => "/")
    (check (path->string (path-parent (path "/tmp/demo.txt"))) => "/tmp/")
    (check (path->string (path-parent (path-parent (path "/tmp/demo.txt")))) => "/")
  ) ;when
  (when (os-windows?)
    (check (path->string (path-parent (path "C:\\Users"))) => "C:\\")
    (check (path->string (path-parent (path "a\\b"))) => "a\\")
  ) ;when
) ;let

#|
path-dir?
判断给定路径是否为目录

函数签名
----
(path-dir? path) → boolean

参数
----
path : string 或 path-value
文件路径。

返回值
----
#t : 路径存在且为目录
#f : 路径不存在或不是目录

特殊情况
----
- "" 空字符串 → #f
- "." 当前目录 → #t
- ".." 上级目录 → #t
|#

;; 基本功能测试
(check (path-dir? ".") => #t)
(check (path-dir? (path ".")) => #t)
(check (path-dir? "..") => #t)

;; 边界情况测试
(check (path-dir? "") => #f)
(check (path-dir? "nonexistent") => #f)
(check (path-dir? "#\\null") => #f)

;; 临时目录测试
(check-true (path-dir? (path-temp-dir)))
(check-true (path-dir? (path->string (path-temp-dir))))

;; 文件测试（不是目录）
(let ((temp-file (temp-path "path-dir-file-check.txt")))
  (delete-file-if-exists temp-file)
  (path-write-text temp-file "demo")
  (check-false (path-dir? temp-file))
  (check-false (path-dir? (path->string temp-file)))
  (delete-file-if-exists temp-file)
) ;let

(when (not (os-windows?))
  ;; 根目录与常用目录测试
  (check (path-dir? "/") => #t)
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/etc") => #t)
  ;; 不存在目录测试
  (check (path-dir? "/no_such_dir") => #f)
  (check (path-dir? "/not/a/real/path") => #f)
) ;when

(when (os-windows?)
  ;; 根目录与常用目录测试
  (check (path-dir? "C:/") => #t)
  (check (path-dir? "C:/Windows") => #t)
  (check (path-dir? "C:/Program Files") => #t)
  ;; 不存在目录测试
  (check (path-dir? "C:/no_such_dir/") => #f)
  (check (path-dir? "Z:/definitely/not/exist") => #f)
) ;when

#|
path-file?
判断给定路径是否为文件

函数签名
----
(path-file? path) → boolean

参数
----
path : string 或 path-value
文件路径。

返回值
----
#t : 路径存在且为文件
#f : 路径不存在或不是文件
|#

;; 基本功能测试
(check (path-file? ".") => #f)
(check (path-file? "..") => #f)
(check (path-file? "") => #f)
(check (path-file? "nonexistent") => #f)

;; 临时文件测试
(let ((temp-file (temp-path "path-file-check.txt")))
  (delete-file-if-exists temp-file)
  (path-write-text temp-file "test content for path-file?")
  (check-true (path-file? temp-file))
  (check-true (path-file? (path->string temp-file)))
  (check-false (path-dir? temp-file))
  (delete-file-if-exists temp-file)
) ;let

(when (not (os-windows?))
  (check (path-file? "/") => #f)
  (check-true (path-file? "/etc/hosts"))
  (check (path-file? "/tmp") => #f)
) ;when

(when (os-windows?)
  (check (path-file? "C:/") => #f)
  (check-true (path-file? "C:/Windows/System32/drivers/etc/hosts"))
  (check (path-file? "C:/Windows") => #f)
) ;when

#|
path-exists?
判断给定路径是否存在。

语法
----
(path-exists? path) → boolean

参数
----
path : string 或 path-value
文件或目录路径。

返回值
-----
boolean
当路径存在时返回 #t，路径不存在时返回 #f。
|#

;; 基本功能测试
(check-true (path-exists? "."))
(check-true (path-exists? ".."))
(check-true (path-exists? (path ".")))

;; 边界情况测试
(check (path-exists? "") => #f)
(check (path-exists? "nonexistent") => #f)
(check (path-exists? "#/null") => #f)

;; 系统路径测试
(when (not (os-windows?))
  (check-true (path-exists? "/"))
  (check-true (path-exists? "/etc"))
  (check-true (path-exists? "/etc/passwd"))
  (check (path-exists? "/no_such_file") => #f)
  (check (path-exists? "/not/a/real/path") => #f)
) ;when

(when (os-windows?)
  (check-true (path-exists? "C:/"))
  (check-true (path-exists? "C:/Windows"))
  (check-true (path-exists? "C:\\Windows\\System32\\drivers\\etc\\hosts"))
  (check (path-exists? "C:\\Windows\\InvalidPath") => #f)
) ;when

;; 临时文件存在性测试
(let ((temp-file (temp-path "path-exists-test.txt")))
  (delete-file-if-exists temp-file)
  (check-false (path-exists? temp-file))
  (path-write-text temp-file "test content")
  (check-true (path-exists? temp-file))
  (check-true (path-exists? (path->string temp-file)))
  (delete-file-if-exists temp-file)
) ;let

#|
path-read-text / path-write-text / path-append-text
读写文本文件。

语法
----
(path-read-text path) → string
(path-write-text path content) → unspecified
(path-append-text path content) → unspecified

描述
----
这些函数对应 `rich-path` 中文件文本读写相关能力的函数式版本。
支持字符串路径与 path-value 两种输入形式。
|#

;; 基本文本读写测试
(let ((text-file (temp-path "path-read-text-basic.txt")))
  (delete-file-if-exists text-file)
  (path-write-text text-file "Hello, World!")
  (check (path-read-text text-file) => "Hello, World!")
  (check (path-read-text (path->string text-file)) => "Hello, World!")
  (delete-file-if-exists text-file)
) ;let

;; 空文件测试
(let ((empty-file (temp-path "path-read-text-empty.txt")))
  (delete-file-if-exists empty-file)
  (path-write-text empty-file "")
  (check (path-read-text empty-file) => "")
  (delete-file-if-exists empty-file)
) ;let

;; 中文文本测试
(let ((chinese-file (temp-path "path-read-text-zh.txt")))
  (delete-file-if-exists chinese-file)
  (path-write-text chinese-file "你好，世界！\n这是一段中文测试文本。")
  (check (path-read-text chinese-file) => "你好，世界！\n这是一段中文测试文本。")
  (delete-file-if-exists chinese-file)
) ;let

;; 追加文本测试
(let ((append-file (temp-path "path-append-text.txt")))
  (delete-file-if-exists append-file)
  (path-write-text append-file "Initial content\n")
  (check (path-read-text append-file) => "Initial content\n")
  (path-append-text append-file "Appended content\n")
  (check (path-read-text append-file)
         => "Initial content\nAppended content\n")
  (delete-file-if-exists append-file)
) ;let

;; 追加到不存在文件测试
(let ((append-missing-file (temp-path "path-append-missing.txt")))
  (delete-file-if-exists append-missing-file)
  (path-append-text append-missing-file "new")
  (check (path-read-text append-missing-file) => "new")
  (delete-file-if-exists append-missing-file)
) ;let

;; 大文件读取测试
(let ((big-file (temp-path "path-read-text-large.txt"))
      (large-content (make-string 10000 #\a)))
  (delete-file-if-exists big-file)
  (path-write-text big-file large-content)
  (let ((read-content (path-read-text big-file)))
    (check (string-length read-content) => 10000)
    (check (string=? read-content large-content) => #t)
  ) ;let
  (delete-file-if-exists big-file)
) ;let

;; 多层目录读取测试
(let* ((base-dir (temp-path "path-read-text-depth"))
       (nested-dir (path-join base-dir "nested"))
       (deep-file (path-join nested-dir "deep.txt")))
  (delete-file-if-exists deep-file)
  (delete-dir-if-exists nested-dir)
  (delete-dir-if-exists base-dir)
  (mkdir (path->string base-dir))
  (mkdir (path->string nested-dir))
  (path-write-text deep-file "Deeply nested file content")
  (check (path-read-text deep-file) => "Deeply nested file content")
  (delete-file-if-exists deep-file)
  (delete-dir-if-exists nested-dir)
  (delete-dir-if-exists base-dir)
) ;let*

;; 错误处理测试
(check-catch 'file-not-found-error (path-read-text (path "/this/file/does/not/exist")))

#|
path-read-bytes
以 bytevector 形式从文件中读取二进制数据。

函数签名
----
(path-read-bytes path) → bytevector

参数
----
path : string 或 path-value
文件路径（可以是绝对路径或相对路径）

返回值
----
bytevector
二进制文件数据
|#

;; 基本二进制文件测试
(let ((binary-file (temp-path "path-read-bytes-basic.dat")))
  (delete-file-if-exists binary-file)
  (path-write-text binary-file "Hello, binary world!")
  (let ((read-content (path-read-bytes binary-file)))
    (check-true (bytevector? read-content))
    (check (bytevector-length read-content) => 20)
    (check (utf8->string read-content) => "Hello, binary world!")
  ) ;let
  (delete-file-if-exists binary-file)
) ;let

;; 空二进制文件测试
(let ((empty-file (temp-path "path-read-bytes-empty.dat")))
  (delete-file-if-exists empty-file)
  (path-write-text empty-file "")
  (let ((empty-bytes (path-read-bytes empty-file)))
    (check (bytevector-length empty-bytes) => 0)
  ) ;let
  (delete-file-if-exists empty-file)
) ;let

;; 中文文件名二进制读取测试
(let ((chinese-binary (temp-path "中文_测试数据.bin")))
  (delete-file-if-exists chinese-binary)
  (path-write-text chinese-binary "\x01\x02\x03\x04\x05")
  (let ((read-chinese (path-read-bytes chinese-binary)))
    (check-true (bytevector? read-chinese))
    (check-true (> (bytevector-length read-chinese) 0))
  ) ;let
  (delete-file-if-exists chinese-binary)
) ;let

;; 与 path-read-text 的对比测试
(let ((comparison-file (temp-path "path-read-bytes-comparison.dat")))
  (delete-file-if-exists comparison-file)
  (path-write-text comparison-file "Hello, World!测试")
  (let ((binary-data (path-read-bytes comparison-file)))
    (check-true (bytevector? binary-data))
    (check (utf8->string binary-data) => "Hello, World!测试")
  ) ;let
  (check (path-read-text comparison-file) => "Hello, World!测试")
  (delete-file-if-exists comparison-file)
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error (path-read-bytes (path "/this/file/does/not/exist")))

#|
path-touch
创建或更新时间戳文件。

语法
----
(path-touch path) → boolean

返回值
-----
boolean
返回 #t 表示操作成功完成。

描述
----
`path-touch` 是 `rich-path` 中文件触碰能力的函数式版本，
用于创建空文件或更新现有文件/目录的时间戳。
|#

;; 创建新空文件测试
(let ((touch-file (temp-path "path-touch-basic.txt")))
  (delete-file-if-exists touch-file)
  (check-false (path-exists? touch-file))
  (check-true (path-touch touch-file))
  (check-true (path-exists? touch-file))
  (check-true (path-file? touch-file))
  (check (path-getsize touch-file) => 0)
  (delete-file-if-exists touch-file)
) ;let

;; 更新现有文件时间戳测试
(let ((touch-file (temp-path "path-touch-update.txt")))
  (delete-file-if-exists touch-file)
  (path-write-text touch-file "keep")
  (check-true (path-touch touch-file))
  (check (path-read-text touch-file) => "keep")
  (delete-file-if-exists touch-file)
) ;let

;; 目录时间戳测试
(let ((touch-dir (temp-path "path-touch-dir")))
  (delete-dir-if-exists touch-dir)
  (mkdir (path->string touch-dir))
  (check-true (path-touch touch-dir))
  (check-true (path-dir? touch-dir))
  (delete-dir-if-exists touch-dir)
) ;let

;; 特殊文件名测试
(let ((special-file (temp-path "path-touch-special_中文#.txt")))
  (delete-file-if-exists special-file)
  (check-true (path-touch special-file))
  (check-true (path-exists? special-file))
  (delete-file-if-exists special-file)
) ;let

;; 相对路径与重复调用测试
(let ((relative-file (path "path-touch-relative.txt")))
  (delete-file-if-exists relative-file)
  (check-true (path-touch relative-file))
  (check-true (path-touch relative-file))
  (check-true (path-touch relative-file))
  (check-true (path-exists? relative-file))
  (delete-file-if-exists relative-file)
) ;let

#|
path-list / path-list-path / path-unlink / path-rmdir
目录列举与删除操作。

语法
----
(path-list path) → vector
(path-list-path path) → vector
(path-unlink path [missing-ok]) → boolean
(path-rmdir path) → boolean

描述
----
这些函数对应 `rich-path` 中 `:list`、`:list-path`、`:unlink`、`:rmdir`
的函数式版本。其中 `path-list` 返回字符串向量，`path-list-path` 返回
路径值向量。
|#

;; 目录列举与删除测试
(let* ((list-dir (temp-path "path-list-dir"))
       (list-file-a (path-join list-dir "child-a.txt"))
       (list-file-b (path-join list-dir "child-b.txt")))
  (delete-file-if-exists list-file-a)
  (delete-file-if-exists list-file-b)
  (delete-dir-if-exists list-dir)
  (mkdir (path->string list-dir))
  (path-write-text list-file-a "a")
  (path-write-text list-file-b "b")

  (check-true (vector? (path-list list-dir)))
  (check-true (string-list-contains? "child-a.txt" (vector->list (path-list list-dir))))
  (check-true (string-list-contains? "child-b.txt" (vector->list (path-list list-dir))))

  (let ((listed-paths (path-list-path list-dir)))
    (check-true (vector? listed-paths))
    (check-true (string-list-contains? (path->string list-file-a)
                                       (path-vector->string-list listed-paths)))
    (check-true (string-list-contains? (path->string list-file-b)
                                       (path-vector->string-list listed-paths)))
  ) ;let

  (check-true (path-unlink list-file-a))
  (check-false (path-exists? list-file-a))
  (check-true (path-unlink list-file-a #t))
  (check-catch 'file-not-found-error (path-unlink list-file-a))

  (check-true (path-unlink list-file-b))
  (check-true (path-rmdir list-dir))
  (check-false (path-exists? list-dir))
) ;let*

#|
path-getsize
获取文件或目录的大小（字节数）。

语法
----
(path-getsize path) → integer

参数
----
path : string 或 path-value
要获取大小的文件或目录路径。

返回值
-----
integer
返回文件或目录的字节大小。
|#

;; 系统路径大小测试
(check-true (> (path-getsize (path-root)) 0))
(when (not (os-windows?))
  (check-true (> (path-getsize "/etc/hosts") 0))
  (check-true (> (path-getsize "/tmp") 0))
) ;when

;; 临时文件大小测试
(let ((size-file (temp-path "path-getsize.txt")))
  (delete-file-if-exists size-file)
  (path-write-text size-file "")
  (check (path-getsize size-file) => 0)
  (path-write-text size-file "test")
  (check (path-getsize size-file) => 4)
  (path-write-text size-file "hello world test content")
  (check (path-getsize size-file) => 24)
  (path-write-text size-file "中文测试")
  (check (path-getsize size-file) => 12)
  (delete-file-if-exists size-file)
) ;let

;; 相对路径文件大小测试
(let ((rel-file (path "path-getsize-relative.txt")))
  (delete-file-if-exists rel-file)
  (path-write-text rel-file "temporary file for testing")
  (check (path-getsize rel-file) => 26)
  (delete-file-if-exists rel-file)
) ;let

;; 错误处理测试
(check-catch 'file-not-found-error (path-getsize (path "/this/file/does/not/exist")))

(check-report)

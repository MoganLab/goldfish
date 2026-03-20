;
; Copyright (C) 2024 The Goldfish Scheme Authors
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
        (liii string)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

#|
path-dir?
判断给定路径是否为目录

函数签名
----
(path-dir? path) → boolean

参数
----
path : 文件路径（string类型）

返回值
----
#t : 路径存在且为目录
#f : 路径不存在或不是目录

特殊情况
----
- "" 空字符串 → #f
- "."  当前目录 → #t（总是存在）
- ".." 上级目录 → #t（总是存在）

跨平台行为
----
在类Unix系统上：根目录 "/" 总是返回 #t
在Windows系统上：驱动器根目录如 "C:\" 总是返回 #t
路径区分大小写：类Unix系统区分大小写，Windows系统不区分

错误处理
----
不存在的路径返回 #f，不会报错
|#

;; 基本功能测试
(check (path-dir? ".") => #t)
(check (path-dir? "..") => #t)

;; 边界情况测试
(check (path-dir? "") => #f)
(check (path-dir? "nonexistent") => #f)
(check (path-dir? "#\null") => #f)

;; 文件测试（不是目录）
(when (or (os-linux?) (os-macos?))
  (check (path-dir? "/etc/passwd") => #f)
) ;when

(when (os-windows?)
  (check (path-dir? "C:\\Windows\\System32\\drivers\\etc\\hosts") => #f)
) ;when

(when (not (os-windows?))
  ;; 根目录测试
  (check (path-dir? "/") => #t)
  ;; 常用目录测试
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/etc") => #t)
  (check (path-dir? "/var") => #t)
  ;; 不存在的目录测试
  (check (path-dir? "/no_such_dir") => #f)
  (check (path-dir? "/not/a/real/path") => #f)
  ;; 相对路径测试
  (check-true (path-dir? (os-temp-dir)))
) ;when

(when (os-windows?)
  ;; 根目录测试
  (check (path-dir? "C:/") => #t)
  (when (path-exists? "D:/")
    (check (path-dir? "D:/") => #t)
  ) ;when
  ;; 常用目录测试
  (check (path-dir? "C:/Windows") => #t)
  (check (path-dir? "C:/Program Files") => #t)
  ;; 不存在的目录测试
  (check (path-dir? "C:/no_such_dir/") => #f)
  (check (path-dir? "Z:/definitely/not/exist") => #f)
  ;; 大小写测试
  (check (path-dir? "C:/WINDOWS") => #t)
  (check (path-dir? "c:/windows") => #t)
) ;when

#|
path-file?
判断给定路径是否为文件

函数签名
----
(path-file? path) → boolean

参数
----
path : 文件路径（string类型）

返回值
----
#t : 路径存在且为文件
#f : 路径不存在或不是文件

特殊情况
----
- "" 空字符串 → #f
- "."  当前目录 → #f（目录不是文件）
- ".." 上级目录 → #f（目录不是文件）

跨平台行为
----
在类Unix系统上：普通文件返回 #t
在Windows系统上：遵循驱动器路径规则
路径区分大小写：类Unix系统区分大小写，Windows系统不区分相关内容

错误处理
----
不存在的路径返回 #f，不会报错

相关函数
----
- path-dir? : 判断是否为目录
- path-exists? : 判断路径是否存在
|#

;; 基本功能测试
(check (path-file? ".") => #f)
(check (path-file? "..") => #f)

;; 边界情况测试
(check (path-file? "") => #f)
(check (path-file? "nonexistent") => #f)
(check (path-file? "#\null") => #f)

;; 文件测试（是文件）
(when (or (os-linux?) (os-macos?))
  (check (path-file? "/etc/passwd") => #t)
  (check (path-file? "/etc/hosts") => #t)
  (check (path-file? "/usr/bin/env") => #t)
) ;when

(when (os-windows?)
  (check (path-file? "C:/Windows/System32/drivers/etc/hosts") => #t)
  (check (path-file? "C:/Windows/win.ini") => #t)
) ;when

(when (not (os-windows?))
  ;; 根目录测试（不是文件）
  (check (path-file? "/") => #f)
  ;; 常用目录测试（不是文件）
  (check (path-file? "/tmp") => #f)
  (check (path-file? "/etc") => #f)
  (check (path-file? "/var") => #f)
  ;; 不存在的文件测试
  (check (path-file? "/no_such_file.txt") => #f)
  (check (path-file? "/not/a/real/file") => #f)
  ;; 相对路径测试
  (check (path-file? (os-temp-dir)) => #f) ; temp-dir是目录
) ;when

(when (os-windows?)
  ;; 根目录测试
  (check (path-file? "C:/") => #f)
  (check (path-file? "D:/") => #f)
  ;; 常用目录测试
  (check (path-file? "C:/Windows") => #f)
  (check (path-file? "C:/Program Files") => #f)
  ;; 不存在的文件测试
  (check (path-file? "C:/no_such_file.txt") => #f)
  (check (path-file? "Z:/definitely/not/exist") => #f)
  ;; 大小写测试
  (check (path-file? "C:/WINDOWS/explorer.exe") => #t)
  (check (path-file? "c:/windows/explorer.exe") => #t)
) ;when

;; 测试临时文件
(let ((test-file (string-append (os-temp-dir) "/test_path_file.txt")))
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; 测试不存在的文件
  (check (path-file? test-file) => #f)
  
  ;; 创建文件
  (with-output-to-file test-file
    (lambda () (display "test content for path-file?"))
  ) ;with-output-to-file
  
  ;; 测试存在的文件
  (check-true (path-file? test-file))
  
  ;; 清理
  (delete-file test-file)
) ;let

;; 测试真实文件
(when (or (os-linux?) (os-macos?))
  (let ((real-file (string-append (os-temp-dir) "/real_file.txt")))
    
    ;; 创建真实文件
    (with-output-to-file real-file
      (lambda () (display "real content"))
    ) ;with-output-to-file
    
    ;; 测试真实文件
    (check (path-file? real-file) => #t)
    
    ;; 清理
    (delete-file real-file)
  ) ;let
) ;when

#|
path-read-bytes
以bytevector形式从文件中读取二进制数据。

函数签名
----
(path-read-bytes path) → bytevector

参数
----
path : string
文件路径（可以是绝对路径或相对路径）

返回值
----
bytevector
二进制文件数据

描述
----
path-read-bytes用于从指定路径的文件中读取二进制数据，并以bytevector形式返回。与path-read-text读取文本数据不同，此函数专门用于读取二进制文件，不会进行字符编码转换，确保数据的完整性。

适用场景
--------
- 读取二进制文件（如图片、音频、视频等）
- 读取结构化数据文件
- 处理需要字节级精确度的文件操作
- 与bytes相关函数联合使用

错误处理
------
- 路径不存在：抛出file-not-found-error
- 权限不足：抛出相应错误
- 内存不足：抛出内存错误

|#

;; 基本二进制文件测试
(let ((test-binary-file (string-append (os-temp-dir) (string (os-sep)) "test_binary.dat")))
  (let ((test-data "\x00\x01\x02\xFF\xFE\xAB\xCD\xEF"))
    ;; 创建二进制文件
    (call-with-output-file test-binary-file
      (lambda (port)
        (write test-data port)
      ) ;lambda
    ) ;call-with-output-file
    
    ;; 测试读取二进制数据
    (let ((read-bytes (path-read-bytes test-binary-file)))
      (check-true (bytevector? read-bytes))
      (check-true (> (bytevector-length read-bytes) 0))
    ) ;let
    
    ;; 清理
    (delete-file test-binary-file)
  ) ;let
) ;let

;; 测试空二进制文件
(let ((empty-file (string-append (os-temp-dir) (string (os-sep)) "empty_binary.dat")))
  ;; 创建空文件
  (call-with-output-file empty-file (lambda (port) #f))
  
  ;; 测试读取空文件
  (let ((empty-bytes (path-read-bytes empty-file)))
    (check (bytevector-length empty-bytes) => 0)
  ) ;let
  
  ;; 清理
  (delete-file empty-file)
) ;let

;; 测试中文文件名二进制读取
(let ((chinese-binary (string-append (os-temp-dir) (string (os-sep)) "中文_测试数据.bin")))
  (let ((data "\x01\x02\x03\x04\x05"))
    ;; 创建中文文件名二进制文件
    (path-write-text chinese-binary data)
    
    ;; 测试中文文件名读取
    (let ((read-chinese (path-read-bytes chinese-binary)))
      (check-true (bytevector? read-chinese))
      (check-true (> (bytevector-length read-chinese) 0))
    ) ;let
    
    ;; 清理
    (delete-file chinese-binary)
  ) ;let
) ;let

;; 测试与path-read-text的对比
(let ((comparison-file (string-append (os-temp-dir) (string (os-sep)) "comparison_test.dat")))
  (let ((text-data "Hello, World!测试"))
    (path-write-text comparison-file text-data)
    
    ;; 测试二进制读取
    (let ((binary-data (path-read-bytes comparison-file)))
      (check-true (bytevector? binary-data))
      (let ((text-from-binary (utf8->string binary-data)))
        (check (string=? text-from-binary text-data) => #t)
      ) ;let
    ) ;let
    
    ;; 测试文本读取作为对比
    (let ((text-data-verify (path-read-text comparison-file)))
      (check (string=? text-data text-data-verify) => #t)
    ) ;let
    
    (delete-file comparison-file)
  ) ;let
) ;let

; Test for path-read-bytes
(let ((file-name "binary-test.dat")
      (file-content "Hello, binary world!"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ; Write a simple string to the file
  (path-write-text file-path file-content)
  
  ; Read it back using path-read-bytes
  (let ((read-content (path-read-bytes file-path)))
    ; Check that it's a bytevector
    (check-true (bytevector? read-content))
    ; Check that it has the correct length
    (check (bytevector-length read-content) => (string-length file-content))
    ; Check that the content matches when converted back to string
    (check (utf8->string read-content) => file-content)
  ) ;let
  
  (delete-file file-path)
) ;let


;; 测试错误处理
(check-catch 'file-not-found-error (path-read-bytes "/this/file/does/not/exist"))

(check-catch 'file-not-found-error (path-read-bytes "/nonexistent"))


;; 测试 path-append-text
(let ((file-name "append-test.txt")
      (initial-content "Initial content\n")
      (append-content "Appended content\n"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ;; 先写入初始内容
  (path-write-text file-path initial-content)
  
  ;; 验证初始内容
  (check (path-read-text file-path) => initial-content)
  
  ;; 追加内容
  (path-append-text file-path append-content)
  
  ;; 验证追加后的内容
  (check (path-read-text file-path) => (string-append initial-content append-content))
  
  ;; 清理
  (delete-file file-path)
) ;let

;; 测试追加到不存在的文件
(let ((file-name "append-new-file.txt")
      (content "Content for new file\n"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  
  ;; 确保文件不存在
  (when (file-exists? file-path)
    (delete-file file-path)
  ) ;when
  
  ;; 追加到不存在的文件
  (path-append-text file-path content)
  
  ;; 验证内容
  (when (or (os-macos?) (os-linux?))
    (check (path-read-text file-path) => content)
  ) ;when
  
  ;; 清理
  (delete-file file-path)
) ;let

(let ((test-file (string-append (os-temp-dir) "/test_touch.txt")))
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; Test creating new file
  (check-true (path-touch test-file))
  (check-true (file-exists? test-file))
  
  ;; Test updating existing file
  (let ((old-size (path-getsize test-file)))
    (check-true (path-touch test-file))
    (check (>= (path-getsize test-file) old-size) => #t)
  ) ;let
  
  ;; Clean up
  (delete-file test-file)
) ;let

(check ((path) :get-type) => 'posix)
(check ((path) :get-parts) => #("."))

#|
path-touch
创建或更新时间戳文件。如果文件已存在则更新其修改和访问时间戳，如果文件不存在则创建一个空文件。

函数签名
----
(path-touch path) ⇒ boolean

参数
----
path : string
文件路径（可以是绝对路径或相对路径）

返回值  
-----
boolean
返回 #t 表示操作成功完成。

描述
----
`path-touch` 用于创建空文件或更新现有文件的时间戳，与Unix/Linux的`touch`命令功能相同。

行为特征
------
- **文件不存在时**：创建一个新的空文件
- **文件已存在时**：更新文件的最后修改时间和访问时间
- **目录路径**：同样支持，会更新目录的时间戳
- **空文件创建**：创建的文件大小为0字节
- **内容不变**：对已存在文件不会修改其内容，只是更新时间

错误处理
------
- **权限错误**：如果无权限创建文件或写入目录，会抛出异常
- **路径无效**：如果父目录不存在，可能抛出异常  
- **磁盘空间**：磁盘空间不足时可能失败

跨平台行为
---------
- **Unix/Linux/macOS**：支持所有文件系统的时间戳操作，需注意目录权限
- **Windows**：支持NTFS、FAT等文件系统的时间戳操作，路径分隔符兼容

应用场景
--------
- 创建用于测试的空文件
- 更新时间戳以触发重新编译或执行某些操作
- 确保文件存在以备后续写入操作
- 创建日志文件或其他需要存在的基础文件

注意事项
--------
1. 目标路径的父目录必须存在，否则可能创建失败
2. 不需要关心文件权限问题，系统会自动使用默认权限
3. 不能在只读挂载的目录上创建文件
4. 支持批量操作，可以多次调用同一文件，不会报错

与Unix touch命令对比
-----------------
`path-touch` 功能与Unix/Linux系统的`touch`命令类似：
- 支持所有兼容平台上的时间戳操作
- 不修改已存在文件的内容
- 创建0字节的文件（和空文件一致）
- 跨平台一致的行为表现

示例
----
```scheme
;; 创建新空文件
(path-touch "newfile.txt")

;; 更新已存在文件的时间戳  
(path-touch "existing.txt")

;; 创建目录中的文件
(path-touch "logs/app.log")

;; 使用时间戳更新作为条件操作
(when (not (file-exists? "temp.txt"))
  (path-touch "temp.txt"))
```

与 path%touch 的区别
-------------------
- `path-touch` 是函数版本，直接操作字符串路径
- `path%touch` 是方法版本，通过路径对象链式调用
- 功能上完全等价，只是使用方式不同
|#

;; 基本功能测试：创建新文件
(let ((test-file (string-append (os-temp-dir) (string (os-sep)) "test_path_touch_basic.txt")))
  ;; 确保文件不存在
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; 测试创建新文件
  (check (path-touch test-file) => #t)
  (check (file-exists? test-file) => #t)
  (check (path-file? test-file) => #t)
  (check (path-getsize test-file) => 0) ; 空文件
  
  ;; 清理
  (delete-file test-file)
) ;let

;; 测试更新现有文件时间戳
(let ((test-file (string-append (os-temp-dir) (string (os-sep)) "test_path_touch_update.txt")))
  ;; 确保文件不存在
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; 创建文件并写入内容
  (path-write-text test-file "test content")
  (let ((original-size (path-getsize test-file)))
    
    ;; 更新时间戳（不应改变内容）
    (check (path-touch test-file) => #t)
    
    ;; 验证文件内容未改变
    (check (path-read-text test-file) => "test content")
    (check (path-getsize test-file) => original-size)
    
    ;; 验证文件仍然存在
    (check (path-exists? test-file) => #t)
  ) ;let
  
  ;; 清理
  (delete-file test-file)
) ;let

;; 测试目录时间戳更新
(let ((test-dir (string-append (os-temp-dir) (string (os-sep)) "test_path_touch_dir")))
  ;; 确保目录存在
  (when (not (file-exists? test-dir))
    (mkdir test-dir)
  ) ;when
  
  ;; 测试更新目录时间戳
  (check (path-touch test-dir) => #t)
  (check (path-exists? test-dir) => #t)
  (check (path-dir? test-dir) => #t)
  
  ;; 清理
  (rmdir test-dir)
) ;let

;; 测试特殊文件名支持
(let ((special-file (string-append (os-temp-dir) (string (os-sep)) "path-touch-special_中文#.txt")))
  ;; 确保文件不存在
  (when (file-exists? special-file)
    (delete-file special-file)
  ) ;when
  
  ;; 测试特殊文件名创建
  (check (path-touch special-file) => #t)
  (check (file-exists? special-file) => #t)
  
  ;; 清理
  (delete-file special-file)
) ;let

;; 测试相对路径创建
(let ((relative-file "./test_relative_path_touch.txt"))
  ;; 确保文件不存在
  (when (file-exists? relative-file)
    (delete-file relative-file)
  ) ;when
  
  ;; 测试相对路径创建
  (check (path-touch relative-file) => #t)
  (check (file-exists? relative-file) => #t)
  
  ;; 清理
  (delete-file relative-file)
) ;let

;; 测试重复调用行为
(let ((repeat-file (string-append (os-temp-dir) (string (os-sep)) "test_repeat_path_touch.txt")))
  ;; 确保文件不存在
  (when (file-exists? repeat-file)
    (delete-file repeat-file)
  ) ;when
  
  ;; 多次调用不应导致错误
  (check (path-touch repeat-file) => #t)
  (check (path-touch repeat-file) => #t)
  (check (path-touch repeat-file) => #t)
  (check (file-exists? repeat-file) => #t)
  (check (path-getsize repeat-file) => 0)
  
  ;; 清理
  (delete-file repeat-file)
) ;let


#|
path@of-drive
根据驱动器字母构造 Windows 盘符根路径。

语法
----
(path :of-drive drive-letter)

参数
----
drive-letter : char
驱动器字母（A-Z，大小写不敏感，但应加 #\ 前缀）。

返回值
-----
string
格式化后的盘符根路径（字母大写 + :\\ 后缀）。

错误
----
type-error
若 drive-letter 不是 char 类型或无法被大写化（非英文字母）。

|#

;; Example of type-error
(check-catch 'type-error (path :of-drive 1 :to-string))

(check (path :of-drive #\D :to-string) => "D:\\")
(check (path :of-drive #\d :to-string) => "D:\\")

(check (path :root :to-string) => "/")

(when (or (os-macos?) (os-linux?))
  (check (path :from-parts #("/" "tmp")) => (path :/ "tmp"))
  (check (path :from-parts #("/" "tmp" "test")) => (path :/ "tmp" :/ "test"))
  (check (path :from-parts #("/", "tmp") :to-string) => "/tmp")
) ;when

(when (os-windows?)
  (check (path :/ "C:" :to-string) => "C:\\")
) ;when

(when (not (os-windows?))
  (check (path :/ "root" :to-string) => "/root")
) ;when

(when (os-windows?)
  (check (path "a\\b") => (path :./ "a" :/ "b"))
  (check (path "C:\\") => (path :of-drive #\C))
  (check (path "C:\\Users") => (path :of-drive #\C :/ "Users"))
) ;when

(when (or (os-linux?) (os-macos?))
  (check (path "a/b") => (path :./ "a" :/ "b"))
  (check (path "/tmp") => (path :/ "tmp"))
  (check (path "/tmp/tmp2") => (path :/ "tmp" :/ "tmp2"))
) ;when

(when (os-linux?)
  (check (path :from-env "HOME" :to-string) => (path :home :to-string))
) ;when

(when (os-windows?)
  (check (path :from-env "USERPROFILE" :to-string) => (path :home :to-string))
) ;when

#|
path%name
获取路径的最终文件或目录名称部分。

语法
----
(path-instance :name)

参数
----
无

返回值
-----
string
返回路径的最终名称部分，即从最后一个路径分隔符到末尾的部分。

描述
----
`path%name` 提取路径中的最终名称部分，忽略前面的所有路径层级。这个名称可以是文件名或最后一个目录名。

行为特征
------
- 返回路径中的最终名称（文件名或目录名）
- 处理空路径、当前目录 "."、上级目录 ".." 等特殊情况
- 对于以 "." 结尾的路径返回空字符串
- 保留完整文件名，包括所有后缀
- 跨平台兼容 Windows、Unix/Linux/macOS 的路径规则

特殊情况
------
- "" 空路径 → 返回空字符串
- "." 当前目录 → 返回空字符串  
- ".." 上级目录 → 返回 ".."
- 以"/"结尾的路径 → 返回空字符串
- 多级路径 → 返回最后一级的完整名称

跨平台行为
---------
- Unix/Linux/macOS: 以 `/` 作为路径分隔符
- Windows: 以 `\` 或 `/` 作为路径分隔符
- 返回结果格式一致，不受平台影响

相关函数
--------
- path%stem: 获取去除后缀的文件名
- path%suffix: 获取文件扩展名
|#

(check (path "file.txt" :name) => "file.txt")
(check (path "archive.tar.gz" :name) => "archive.tar.gz") 
(check (path ".hidden" :name) => ".hidden") 
(check (path "noext" :name) => "noext")    
(check (path "" :name) => "")  ; 空路径
(check (path "." :name) => "")  ; 当前目录
(check (path ".." :name) => "..")  ; 上级目录

(when (or (os-macos?) (os-linux?))
  (check (path "/path/to/file.txt" :name) => "file.txt")
) ;when

#|
path%stem
获取路径的stem（去掉最后一个后缀的文件名部分）。

语法
----
(path-instance :stem)

参数
----
无

返回值
-----
string
返回去掉最后一个后缀的文件名部分（stem）。

描述
----
`path%stem` 提取文件名中去掉最后一个扩展名的部分。这在处理文件时非常有用，特别是需要获取"基本文件名"而不关心其扩展名的情况。

行为特征
------
- 保留隐藏文件（以点开头）的完整名称
- 只去掉最后一个扩展名（如 "archive.tar.gz" → "archive.tar"）
- 无扩展名的文件返回原名
- 正确处理特殊目录名称（"." 和 ".."）

扩展名规则
------
1. 文件名包含点号时需考虑多种情况
2. 以点开头的隐藏文件视为无扩展名
3. 多个点号的情况，只识别最后一个点号之后的部分为扩展名

错误处理
------
不返回错误，对于所有输入都会返回合理的字符串结果。

跨平台行为
---------
路径分隔符和扩展名规则在所有平台上保持一致。
|#

;; 基本功能测试
(check (path "file.txt" :stem) => "file")
(check (path "archive.tar.gz" :stem) => "archive.tar")
(check (path ".hidden" :stem) => ".hidden")
(check (path "noext" :stem) => "noext")
(check (path "" :stem) => "")
(check (path "." :stem) => "")
(check (path ".." :stem) => "..")

;; 扩展的测试案例
(check (path "script.bin.sh" :stem) => "script.bin")
(check (path "image.jpeg" :stem) => "image")
(check (path "README" :stem) => "README")
(check (path "config.yaml.bak" :stem) => "config.yaml")
(check (path "test-file.name-with-dots.txt" :stem) => "test-file.name-with-dots")

;; 隐藏文件测试
(check (path ".gitignore" :stem) => ".gitignore")
(check (path ".bashrc" :stem) => ".bashrc")
(check (path ".profile" :stem) => ".profile")

;; 复杂路径测试
(when (or (os-linux?) (os-macos?))
  (check (path "/usr/bin/file.txt" :stem) => "file")
  (check (path "/path/to/archive.tar.gz" :stem) => "archive.tar")
  (check (path "/home/user/.hidden" :stem) => ".hidden")  
) ;when

#|
path%suffix
获取路径的后缀（扩展名）部分。

语法
----
(path-instance :suffix)

参数
----
无

返回值
-----
string
返回文件名的后缀部分，包括点号(.)。如果没有后缀则返回空字符串。

描述
----
`path%suffix` 提取文件名中的最后一个扩展名部分。这对于处理文件类型非常有用，特别是需要根据文件扩展名执行不同操作的情况。

行为特征
------
- 只返回最后一个扩展名（如 "archive.tar.gz" → ".gz"）
- 隐藏文件（以点开头）视为无扩展名
- 无扩展名的文件返回空字符串
- 正确处理特殊目录名称（"." 和 ".."）

扩展名规则
------
1. 文件名包含点号时，最后一个点号之后的部分为扩展名
2. 以点开头的隐藏文件（如 ".hidden"）视为无扩展名
3. 多个点号的情况，只识别最后一个点号之后的部分为扩展名

错误处理
------
不返回错误，对于所有输入都会返回合理的字符串结果。

跨平台行为
---------
路径分隔符和扩展名规则在所有平台上保持一致。
|#

(check (path "file.txt" :suffix) => ".txt")
(check (path "archive.tar.gz" :suffix) => ".gz")  ; 只保留最后一个后缀
(check (path ".hidden" :suffix) => "")  
(check (path "noext" :suffix) => "")  
(check (path "/path/to/file.txt" :suffix) => ".txt")  ; 绝对路径
(check (path "C:/path/to/file.txt" :suffix) => ".txt")  ; Windows路径
(check (path "" :suffix) => "")  ; 空路径
(check (path "." :suffix) => "")  ; 当前目录
(check (path ".." :suffix) => "")  ; 上级目录

(check-true ((path "/tmp/test") :equals (path "/tmp/test")))

#|
path%file?
判断路径所指向文件是否存在。

语法
----
(path-instance :file?)

参数
----
无显式参数（通过 path 对象隐式操作文件系统）。

返回值
-----
boolean
#t: 路径存在且为文件
#f: 路径不存在或不是文件

错误
----
无（不存在的路径返回 #f 而非报错）。

示例
----
(path "file.txt" :file?)        ; 相对路径文件测试
(path "/etc/hosts" :file?)      ; 绝对路径文件测试
(path "/usr/bin" :file?)        ; 目录路径返回 #f

|#

(when (or (os-linux?) (os-macos?))
  (check-false (path :/ "tmp" :file?))
  (chdir "/tmp")
  (mkdir "tmpxxxx") 
  (check-false (path :from-parts #("/" "tmp" "/" "tmpxxxx") :file?))
  (rmdir "tmpxxxx")
) ;when

(when (or (os-linux?) (os-macos?))
  (check-false (path :/ "tmp" :file?))
  (check-true (path :/ "etc" :/ "hosts" :file?))
  (check-false (path :from-parts #("/" "tmpxxxx" "file.txt") :file?))
  (chdir "/tmp")
  (let ((test-file (path "test_path_file.txt")))
    (test-file :write-text "test content")
    (check-true (test-file :file?))
    (test-file :unlink)
  ) ;let
) ;when

(when (os-windows?)
  ;; 基本文件检测
  (check-true (path :from-parts #("C:" "Windows" "win.ini") :file?))
  (check-true (path :from-parts #("C:" "Windows" "System32" "drivers" "etc" "hosts") :file?))
  
  ;; 目录不是文件
  (check-false (path :from-parts #("C:" "Windows") :file?))
  
  ;; 不存在的文件
  (check-false (path :from-parts #("C:" "Windows" "InvalidFile.txt") :file?))
) ;when

#|
path%dir?
判断路径所指向目录是否存在。

语法
----
(path :from-parts string-vector :dir?) ; 通过字符串数组显式构造路径对象
示例：(path :from-parts #("/" "tmp" "log")) ; ⇒ /tmp/log
(path :/ segment1 segment2 ...) ; 构建 Unix 风格绝对路径
示例：(path :/ "tmp" "app.log") ; ⇒ /tmp/app.log

参数
----
无显式参数（通过 path 对象隐式操作文件系统）。

返回值
-----
boolean
#t: 路径存在且为目录
#f: 路径不存在或不是目录

错误
----
无（不存在的路径返回 #f 而非报错）。

|#

(when (or (os-linux?) (os-macos?))
  (check-true (path :/ "tmp" :dir?))
  (check-true (path :/ "tmp/" :dir?))
  (check-false (path :from-parts #("/" "tmpxxxx") :dir?))
  (check-true (path :from-parts #("/" "tmp" "") :dir?))
  (chdir "/tmp")
  (mkdir "tmpxxxx")
  (check-true (path :from-parts #("/" "tmp" "/" "tmpxxxx" "") :dir?))
  (rmdir "tmpxxxx")
) ;when

(when (os-windows?)
  ;; 基本目录检测
  (check-true (path :from-parts #("C:" "Windows") :dir?))
  (check-true (path :from-parts #("C:\\" "Windows\\") :dir?))
  
  ;; 大小写不敏感测试
  (check-true (path :from-parts #("C:" "WINDOWS") :dir?))
  
  ;; 不存在的路径
  (check-false (path :from-parts #("C:" "Windows\\InvalidPath") :dir?))
  
  ;; 带空格的路径
  (check-true (path :from-parts #("C:" "Program Files") :dir?))
  
  ;; 特殊目录（需存在）
  (check-true (path :from-parts #("C:" "Windows" "System32") :dir?))
) ;when

(when (or (os-linux?) (os-macos?))
  (check-true (path :/ "tmp" :exists?))
) ;when

(when (not (os-windows?))
  (check (path :/ "etc" :/ "passwd" :to-string) => "/etc/passwd")
) ;when

(when (os-windows?)
  (check (path :of-drive #\C :to-string) => "C:\\")
) ;when

#|
path-exists?
判断给定路径是否存在。

语法
----
(path-exists? path) → boolean

参数
----
**path** : string
文件或目录路径。

返回值
-----
boolean
当路径存在时返回 #t，路径不存在时返回 #f。

描述
----
`path-exists?` 用于判断指定的文件或目录是否存在，而不会因路径不存在或格式错误而报错。

行为特征
------
- "" (空字符串) → #f
- "." (当前目录) → #t（总是存在）
- ".." (上级目录) → #t（总是存在）
- 路径区分大小写：类Unix系统区分大小写，Windows系统不区分

跨平台行为
----------
- **类Unix系统**：根目录 "/" 总是返回 #t
- **Windows系统**：驱动器根目录如 "C:\" 总是返回 #t

应用场景
--------
- 文件操作前检查文件是否存在，避免错误
- 条件判断中决定是否需要进行文件操作
- 与其他path函数配合使用，构建健壮的filesystem代码

错误处理
------
该函数不会因为路径不存在或格式不正确而报错，而是返回 #f。
|#

;; 基本功能测试
(check-true (path-exists? "."))
(check-true (path-exists? ".."))

;; 边界情况测试
(check (path-exists? "") => #f)
(check (path-exists? "nonexistent") => #f)
(check (path-exists? "#/null") => #f)

;; 文件存在性测试
(when (or (os-linux?) (os-macos?))
  (check-true (path-exists? "/etc/passwd"))
  (check-true (path-exists? "/usr/bin/env"))
) ;when

(when (os-windows?)
  (check-true (path-exists? "C:\\Windows\\System32\\drivers\\etc\\hosts"))
  (check-true (path-exists? "C:\\Windows\\System32\\ntoskrnl.exe"))
) ;when

;; 目录存在性测试
(when (not (os-windows?))
  ;; 根目录测试
  (check-true (path-exists? "/"))
  ;; 系统目录测试
  (check-true (path-exists? "/etc"))
  (check-true (path-exists? "/var"))
  (check-true (path-exists? "/tmp"))
) ;when

(when (os-windows?)
  ;; 盘符根目录测试
  (check-true (path-exists? "C:/"))
  (check-true (path-exists? "C:\\"))
  ;; 系统目录测试
  (check-true (path-exists? "C:/Windows"))
  (check-true (path-exists? "C:\\Program Files"))
) ;when

;; 不存在的路径测试
(when (not (os-windows?))
  (check (path-exists? "/no_such_file") => #f)
  (check (path-exists? "/not/a/real/path") => #f)
  (check (path-exists? "/tmp/nonexistent.txt") => #f)
) ;when

(when (os-windows?)
  (check (path-exists? "C:\\no_such_file") => #f)
  (check (path-exists? "C:\\Windows\\InvalidPath") => #f)
  (check (path-exists? "Z:\\not_a_drive") => #f)
) ;when

;; 相对路径测试
(let ((temp-dir (os-temp-dir)))
  (let ((test-file (string-append temp-dir (string (os-sep)) "path_exists_test.txt")))
    ;; 创建临时文件
    (when (not (file-exists? test-file))
      (with-output-to-file test-file
        (lambda () (display "test content"))
      ) ;with-output-to-file
    ) ;when
    
    ;; 测试文件存在性
    (check-true (path-exists? test-file))
    
    ;; 测试目录存在性
    (check-true (path-exists? temp-dir))
    
    ;; 清理
    (when (file-exists? test-file)
      (delete-file test-file)
    ) ;when
  ) ;let
) ;let

;; 临时文件和目录测试
(let ((temp-file (path :temp-dir :/ "test_exists.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink)
  ) ;when
  
  ;; 测试文件不存在
  (check-false (path-exists? (temp-file :to-string)))
  
  ;; 创建文件
  (temp-file :write-text "test content")
  
  ;; 测试文件存在
  (check-true (path-exists? (temp-file :to-string)))
  
  ;; 测试目录存在
  (check-true (path-exists? ((path :temp-dir) :to-string)))
  
  ;; 清理
  (temp-file :unlink)
) ;let

;; 大小写敏感性和空白字符测试
(when (os-windows?)
  (check-true (path-exists? "C:/windows"))
  (check-true (path-exists? "c:/WINDOWS"))
) ;when

;; 空字符和特殊字符测试
(check (path-exists? "#\null") => #f)
(when (not (os-windows?))
  (check (path-exists? "  ") => #f)  ; 空白字符
) ;when

;; TODO: 在Windows上，空字符串和空白字符串都返回 #t
(when (os-windows?)
  (check (path-exists? " ") => #t)
) ;when


;; 方法链式调用测试 (path对象的%exists?方法)
(check-true ((path ".") :exists?))
(check-true ((path "..") :exists?))
(when (or (os-linux?) (os-macos?))
  (check-true ((path :/ "etc") :exists?))
) ;when

(when (or (os-linux?) (os-macos?))
  (check-false ((path :/ "nonexistent") :exists?))
) ;when

;; path-exists? 与其他函数配合使用测试
(let ((test-file "test_combined_usage.txt"))
  ;; 确保文件不存在
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; 结合path-exists?进行条件操作
  (check-false (path-exists? test-file))
  
  ;; 创建文件
  (path-touch test-file)
  (check-true (path-exists? test-file))
  
  ;; 验证文件大小（使用path-getsize需要文件存在）
  (check-true (>= (path-getsize test-file) 0))
  
  ;; 清理
  (delete-file test-file)
) ;let

;; Windows专用路径分隔符测试
(when (os-windows?)
  (check-true (path-exists? "C:\\"))
  (check-true (path-exists? "C:\\Users"))
) ;when

;; path%append-text 测试
(let ((p (path :temp-dir :/ "append_test.txt")))
  ;; 确保文件不存在
  (when (p :exists?) (p :unlink))
  
  ;; 测试追加到新文件
  (p :append-text "First line\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "First line\n")
  ) ;when
  
  ;; 测试追加到已有文件
  (p :append-text "Second line\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "First line\nSecond line\n")
  ) ;when
  
  ;; 清理
  (p :unlink)
) ;let

(let ((p (path :temp-dir :/ "append_test.txt"))
      (p-windows (path :temp-dir :/ "append_test_windows.txt")))
  ;; 确保文件不存在
  (when (p :exists?) (p :unlink))
  (when (p-windows :exists?) (p-windows :unlink))
  
  (p :append-text "Line 1\n")
  (p-windows :append-text "Line 1\r\n")
  (when (or (os-linux?) (os-macos?))
    (check (p :read-text) => "Line 1\n")
  ) ;when
  (when (os-windows?)
    (check (p-windows :read-text) => "Line 1\r\n")
  ) ;when
  
  ;; 清理
  (p :unlink)
  (p-windows :unlink)
) ;let

(when (not (os-windows?))
  (check (path :/ "etc" :/ "host" :to-string) => "/etc/host")
  (check (path :/ (path "a/b")) => (path "/a/b"))
) ;when

(check-catch 'value-error (path :/ (path "/a/b")))

(when (or (os-linux?) (os-macos?))
  (check (path "/" :parent :to-string) => "/")
  (check (path "" :parent :to-string) => ".")
  (check (path "/tmp/" :parent :to-string) => "/")
  (check (path "/tmp/test" :parent :parent :to-string) => "/")
  (check (path "tmp/test" :parent :to-string) => "tmp/")
  (check (path "tmp" :parent :to-string) => ".")
  (check (path "tmp" :parent :parent :to-string) => ".")
) ;when

(when (os-windows?)
  (check (path "C:" :parent :to-string) => "C:\\")
  (check (path "C:\\Users" :parent :to-string) => "C:\\")
  (check (path "a\\b" :parent :to-string) => "a\\")
) ;when

(when (or (os-macos?) (os-linux?))
  ;; 测试删除文件
  (let ((test-file (string-append (os-temp-dir) "/test_delete.txt")))
    ;; 创建临时文件
    (with-output-to-file test-file
      (lambda () (display "test data"))
    ) ;with-output-to-file
    ;; 验证文件存在
    (check-true (file-exists? test-file))
    ;; 删除文件（使用 remove）
    (check-true (remove test-file))
    ;; 验证文件已删除
    (check-false (file-exists? test-file))
  ) ;let
) ;when

(when (or (os-macos?) (os-linux?))
  ;; 测试删除目录
  (let ((test-dir (string-append (os-temp-dir) "/test_delete_dir")))
    ;; 创建临时目录
    (mkdir test-dir)
    ;; 验证目录存在
    (check-true (file-exists? test-dir))
    ;; 删除目录（使用 rmdir）
    (check-true (rmdir test-dir))
    ;; 验证目录已删除
    (check-false (file-exists? test-dir))
  ) ;let
) ;when

(when (or (os-macos?) (os-linux?))
  ;; 测试 path 对象的 :unlink 和 :rmdir
  (let ((test-file (string-append (os-temp-dir) "/test_path_unlink.txt")))
    (with-output-to-file test-file
      (lambda () (display "test data"))
    ) ;with-output-to-file
    (check-true ((path test-file) :unlink))
    (check-false (file-exists? test-file))
  ) ;let
) ;when

(when (or (os-macos?) (os-linux?))
  (let ((test-dir (string-append (os-temp-dir) "/test_path_rmdir")))
    (mkdir test-dir)
    (check-true ((path test-dir) :rmdir))
    (check-false (file-exists? test-dir))
  ) ;let
) ;when

(when (or (os-macos?) (os-linux?))
  ;; 测试各种调用方式
  (let ((test-file "/tmp/test_unlink.txt"))
    ;; 默认行为 (missing-ok=#f)
    (check-catch 'file-not-found-error
                 ((path test-file) :unlink)
    ) ;check-catch
  
    ;; 显式指定 missing-ok=#t
    (check-true ((path test-file) :unlink #t))
  
    ;; 文件存在时的测试
    (with-output-to-file test-file
      (lambda () (display "test"))
    ) ;with-output-to-file
    (check-true ((path test-file) :unlink))
    (check-false (file-exists? test-file))
  ) ;let
) ;when

(check (path :./ "a" :to-string) => "a")

(when (not (os-windows?))
  (check (path :./ "a" :/ "b" :/ "c" :to-string) => "a/b/c")
) ;when

(when (or (os-linux?) (os-macos?))
  (check-true (path :cwd :dir?))
) ;when

(when (or (os-linux?) (os-macos?))
  (check ((path :home) :to-string) => (getenv "HOME"))
) ;when

(when (os-windows?)
  (check (path :home)
   =>    (path :/ (getenv "HOMEDRIVE") :/ "Users" :/ (getenv "USERNAME"))
  ) ;check
) ;when


#|
path@tempdir
构造指向系统临时目录的 path 对象。

语法
----
(path :temp-dir)

描述
----
该函数返回一个指向操作系统临时目录的 path 对象。

在不同平台上表现：
- **Unix/Linux/macOS**: 通常为 `/tmp`
- **Windows**: 通常为 `C:\Users\[用户名]\AppData\Local\Temp`

返回值
-----
**path对象**
指向系统临时目录的 path 对象，可作为其他路径操作的基础进行链式调用。

特性
---
- 返回的 path 对象总是指向存在的目录
- 跨平台兼容性良好
- 可以进行链式操作

注意事项
------
- 返回的 path 对象始终指向有效目录，不需要检查目录存在性
- 在不同的操作系统会话中，系统临时目录可能会发生变化
- path对象是可变的，但临时目录路径通常保持不变
|#

;; 测试 path@tempdir 方法（path:temp-dir）
(let1 temp-path (path :temp-dir)
  ;; 验证返回的是 path 对象
  (check-true (path :is-type-of temp-path))

  ;; 验证路径存在且是目录
  (check-true (temp-path :exists?))
  (check-true (temp-path :dir?))

  ;; 验证路径与 os-temp-dir 一致
  (check (temp-path :to-string) => (os-temp-dir))

  ;; 验证在不同平台下的基本特征
  (when (os-windows?)
    (check-true (string-starts? (temp-path :to-string) "C:\\"))
  ) ;when

  (when (or (os-linux?) (os-macos?))
    (check-true (string-starts? (temp-path :to-string) "/"))
  ) ;when

  ;; 验证可以进行文件操作
  (let ((test-file (temp-path :/ "path_temp_dir_test.txt")))
    ;; 确保文件不存在
    (when (test-file :exists?)
      (test-file :unlink)
    ) ;when
    
    ;; 测试创建文件
    (test-file :write-text "test content")
    (check-true (test-file :exists?))
    (check (test-file :read-text) => "test content")
    
    ;; 清理
    (test-file :unlink)
  ) ;let

  ;; 验证临时目录内部路径构造正确
  (let ((subdir (temp-path :/ "test_sub_directory")))
    ;; 验证路径构造
    (when (or (os-linux?) (os-macos?))
      (check (subdir :to-string) => (string-append (os-temp-dir) "/test_sub_directory"))
    ) ;when
    
    (when (os-windows?)
      (check (subdir :to-string) => (string-append (os-temp-dir) "\\test_sub_directory"))
    ) ;when
  ) ;let
  
  ;; 验证相对路径操作  
  (let ((rel-path (path "relative")))
    (check-false (rel-path :absolute?))
  ) ;let
) ;let1

#|
path-getsize
获取文件或目录的大小（字节数）。

语法
----
(path-getsize path)

参数
----
path : string
要获取大小的文件或目录路径。路径可以是绝对路径或相对路径。

返回值
-----
integer
返回文件或目录的大小（以字节为单位）。

描述
----
`path-getsize` 用于获取指定文件或目录的字节大小。如果路径指向文件，则返回文件大小；如果指向目录，则返回目录本身的大小（通常很小）。

行为特征
------
- 对于存在的文件，返回其真实字节大小
- 对于目录，返回目录项元数据的大小（不是内容总大小）
- 不能用于获取目录内所有文件的总大小
- 跨平台行为一致

错误处理
------
- 如果路径不存在，函数会抛出 `'file-not-found-error` 错误
- 对于空文件返回 0 字节
- 不会返回负值或无效结果

跨平台行为
----------
- Unix/Linux/macOS 和 Windows 行为一致
- 路径分隔符不影响结果
- 支持 Unicode 文件名

注意事项
------
- 结果始终为非负整数
- 对于大文件可能返回大整数值
- 目录大小指目录本身元数据大小，而非内容总大小
|#

;; 基本功能测试
(check-true (> (path-getsize "/") 0))
(when (not (os-windows?))
  (check-true (> (path-getsize "/etc/hosts") 0))
) ;when

;; 路径对象方法测试
(let ((temp-file (path :temp-dir :/ "test_getsize.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink)
  ) ;when
  
  ;; 测试空文件
  (temp-file :write-text "")
  (check (path-getsize (temp-file :to-string)) => 0)
  
  ;; 测试小文件
  (temp-file :write-text "test")
  (check (path-getsize (temp-file :to-string)) => 4)
  
  ;; 测试较大内容
  (temp-file :write-text "hello world test content")
  (check (path-getsize (temp-file :to-string)) => 24)
  
  ;; 测试中文内容
  (temp-file :write-text "中文测试")
  (check (path-getsize (temp-file :to-string)) => 12)
  
  ;; 清理
  (temp-file :unlink)
) ;let

;; 测试文件不存在错误
(check-catch 'file-not-found-error
  (path-getsize "/nonexistent/path/file.txt")
) ;check-catch

;; 测试现有文件大小
(when (or (os-linux?) (os-macos?))
  (check-true (> (path-getsize "/etc/passwd") 0))
) ;when

(when (os-windows?)
  (check-true (> (path-getsize "C:\\Windows\\System32\\drivers\\etc\\hosts") 0))
) ;when

;; 目录大小测试
(when (or (os-linux?) (os-macos?))
  (check-true (> (path-getsize "/tmp") 0))
) ;when

;; 相对路径测试
(let ((rel-file "test_rel.txt"))
  (when (file-exists? rel-file)
    (delete-file rel-file)
  ) ;when
  
  (with-output-to-file rel-file
    (lambda () (display "temporary file for testing"))
  ) ;with-output-to-file
  
  (check (path-getsize rel-file) => 26)
  
  (delete-file rel-file)
) ;let

;; 测试可以基于临时目录创建文件
(let ((temp-file (path :temp-dir :/ "test_file.txt")))
  ;; 写入测试文件
  (temp-file :write-text "test content")
  
  ;; 验证文件存在
  (check-true (temp-file :exists?))
  (check-true (temp-file :file?))
  
  ;; 清理
  (temp-file :unlink)
) ;let

#|
path%absolute?
判断路径是否为绝对路径。

语法
----
(path-instance :absolute?) → boolean

返回值
-----
boolean
- #t : 路径是绝对路径
- #f : 路径是相对路径

描述
----
`path%absolute?` 用于判断当前 `path` 对象所表示的路径是否为绝对路径。

绝对路径定义
----------

**Unix/Linux/macOS**：
- 以斜杠 `/` 开头的路径为绝对路径
- 示例：`/home/user/file.txt`、`/etc/passwd`

**Windows**：
- 以驱动器字母加冒号和反斜杠开头的路径为绝对路径
- 示例：`C:\Users\user\file.txt`、`D:\data\test.txt`
- 示例：`C:/Users/user/file.txt`（正斜杠也支持）

相对路径特征
----------

**Unix/Linux/macOS**：
- 不以 `/` 开头的路径
- 示例：`file.txt`、`./subdir/file.txt`、`../parent/file.txt`

**Windows**：
- 不以驱动器字母加冒号开头的路径
- 示例：`file.txt`、`subdir\file.txt`、`..\parent\file.txt`

跨平台行为
----------

该函数会自动识别平台类型并应用相应的绝对路径判断标准：
- 在类Unix系统上：只检查是否以 `/` 开头
- 在Windows系统上：检查是否包含有效的驱动器字母格式

错误处理
------
该函数不会抛出错误，对于任何有效的 `path` 对象都会返回明确的布尔值结果。

相关函数
--------
- `path%relative?` : 判断是否为相对路径（`path%absolute?` 的逻辑取反）
- `path@cwd` : 获取当前工作目录的绝对路径
- `path@home` : 获取用户主目录的绝对路径

使用注意事项
------------
1. 空路径对象（`path`）返回 `#f`（相对路径）
2. 路径字符串中的大小写不影响绝对路径判断
3. 路径可以是真实存在也可以是虚拟的，不影响绝对性判断
4. Windows系统中的网络路径（如 `\\server\share`）被视为相对路径
|#

;; 基本绝对路径测试
(check-false ((path) :absolute?))              ; 空路径是相对路径

(when (not (os-windows?))
  (check-true ((path :/ "file.txt") :absolute?)) ; Unix/Linux风格绝对路径
  (check-true ((path :/ "/tmp") :absolute?))    ; 根目录路径
) ;when

;; Windows风格绝对路径测试
(check-true ((path :of-drive #\C) :absolute?)) ; C盘根目录
(check-true ((path :of-drive #\C :/ "Users") :absolute?)) ; Windows完整路径
(check-true ((path :of-drive #\c :/ "data") :absolute?)) ; 小写驱动器字母也支持

;; Unix/Linux/macOS风格绝对路径测试
(when (or (os-linux?) (os-macos?))
  (check-true ((path :/ "etc") :absolute?))
  (check-true ((path :/ "usr" :/ "bin") :absolute?))
  (check-true ((path :/ "home" :/ "user" :/ "documents") :absolute?))
  (check-true ((path :/ "") :absolute?)) ; 根目录本身是绝对的
  (check-true ((path :/ ".") :absolute?)) ; "当前目录" 形式的绝对路径
) ;when

;; Windows风格绝对路径（在Windows系统上测试）
(when (os-windows?)
  (check-true ((path "C:\\Windows") :absolute?))
  (check-true ((path "D:\\data\\file.txt") :absolute?))
  (check-true ((path "C:\\Program Files") :absolute?))
  (check-true ((path :of-drive #\Z :/ "projects") :absolute?))
) ;when

;; 相对路径测试
(check-false ((path :./ "file.txt") :absolute?))        ; 当前目录相对路径
(check-false ((path :./ "dir" :/ "file.txt") :absolute?)) ; 多级相对路径
(check-false ((path "relative.txt") :absolute?))        ; 默认相对路径
(check-false ((path "subdir" :/ "file.txt") :absolute?)) ; 子目录中的文件

;; 上级目录路径测试
(check-false ((path ".." :/ "file.txt") :absolute?))     ; 上级目录相对路径
(check-false ((path :./ ".." :/ "parent") :absolute?))  ; 复杂的相对路径

;; 空路径和各种边界情况测试
(check-false ((path "") :absolute?))                    ; 空字符串路径
(let ((empty-path (path)))          ; 创建空path对象
  (check-false (empty-path :absolute?))
) ;let

;; 路径嵌套中的绝对路径测试
(when (or (os-linux?) (os-macos?))
  (check-true ((path :/ "tmp" :/ "subdir") :absolute?))
  (check-true ((path :/ "var" :/ "log" :/ "syslog") :absolute?))
) ;when

;; 绝对路径链式操作验证
(when (or (os-linux?) (os-macos?))
  (let ((abs-path (path :/ "usr" :/ "local" :/ "bin")))
    (check-true (abs-path :absolute?))
    (check-true ((abs-path :parent) :absolute?)) ; 绝对路径的父目录也是绝对
  ) ;let
) ;when

;; Windows相对路径测试
(when (os-windows?)
  (check-false ((path "file.txt") :absolute?))
  (check-false ((path "subdir\\file.txt") :absolute?))
  (check-false ((path "..\\parent\\file.txt") :absolute?))
) ;when

;; 跨平台路径构建测试
(let ((user-path (path :home :/ "documents" :/ "file.txt")))
  (check-true (user-path :absolute?)) ; home目录产生的路径是绝对的
) ;let

;; 从环境中获取的路径测试
(when (or (os-linux?) (os-macos?))
  (let ((env-path (path :from-env "HOME")))
    (check-true (env-path :absolute?))
  ) ;let
) ;when

(when (os-windows?)
  (let ((env-path (path :from-env "USERPROFILE")))
    (check-true (env-path :absolute?))
  ) ;let
) ;when

;; 临时目录测试
(let ((temp-file (path :temp-dir :/ "test_file.txt")))
  (check-true (temp-file :absolute?)) ; 临时目录产生的路径也是绝对的
) ;let

;; 绝对路径与相对路径的对比测试
(let ((abs-path ((path :home) :to-string))
      (rel-path "file.txt"))
  ;; 通过字符串验证绝对性
  (when (or (os-linux?) (os-macos?))
    (check-true (string-starts? abs-path "/"))
  ) ;when
  
  (check-false (string-starts? rel-path "/"))
) ;let

(check-false ((path) :absolute?))
(check (path :/ "C:" :get-type) => 'windows)
(check (path :/ "C:" :get-parts) => #())
(check-true (path :/ "C:" :absolute?))
(check-true (path :from-parts #("/" "tmp") :absolute?))
(check-false (path :from-parts #("tmp") :absolute?))

#|
path%read-text
从路径对象所指向的文件中读取文本内容。

语法
----
(path-instance :read-text) → string

返回值
-----
string
返回文件的全部文本内容，编码默认为UTF-8。

描述
----
`path%read-text` 是 `path-read-text` 的面向对象版本，用于从路径对象所指向的文件中读取全部文本内容并将其作为字符串返回。

行为特征
------
- 文件必须存在且可读取
- 如果文件不存在会抛出 `file-not-found-error` 错误
- 返回完整的文件内容作为单个字符串
- 支持任意文本文件，包括空文件
- 支持各种字符编码格式的文本文件

错误处理
------
- **file-not-found-error**: 当路径所指向的文件不存在时抛出
- 其他IO错误：包括权限不足、磁盘故障等

与path-read-text的关系
-------------
`path%read-text` 是 `path-read-text` 的面向对象版本：
- `(path-file :read-text)` 等同 `(path-read-text (path-file :to-string))`
- 但使用面向对象的语法更加简洁直观

跨平台行为
---------
- Unix/Linux/macOS：支持所有文件系统的文本读取
- Windows：支持NTFS、FAT等文件系统的文本读取，路径分隔符影响结果

注意事项
--------
1. 读取大文件时请谨慎使用，会装入整个文件到内存
2. 对二进制文件使用会导致数据损坏或错误
3. 始终确保文件存在再读取以避免异常
|#

;; 基本的path%read-text测试
(let ((test-file (path :temp-dir :/ "test_read_text.txt")))
  ;; 确保文件不存在
  (when (test-file :exists?)
    (test-file :unlink)
  ) ;when
  
  ;; 创建测试文件
  (test-file :write-text "Hello, World!")
  
  ;; 读取文件内容
  (check (test-file :read-text) => "Hello, World!")
  
  ;; 清理
  (test-file :unlink)
) ;let

;; 测试读取空文件
(let ((empty-file (path :temp-dir :/ "empty.txt")))
  ;; 确保文件不存在
  (when (empty-file :exists?)
    (empty-file :unlink)
  ) ;when
  
  ;; 创建空文件
  (empty-file :write-text "")
  
  ;; 读取空文件
  (check (empty-file :read-text) => "")
  
  ;; 清理
  (empty-file :unlink)
) ;let

;; 测试读取中文文本
(let ((chinese-file (path :temp-dir :/ "zh_cn.txt")))
  ;; 确保文件不存在
  (when (chinese-file :exists?)
    (chinese-file :unlink)
  ) ;when
  
  ;; 创建中文内容文件
  (chinese-file :write-text "你好，世界！\n这是一段中文测试文本。")
  
  ;; 读取中文内容
  (check (chinese-file :read-text) => "你好，世界！\n这是一段中文测试文本。")
  
  ;; 清理
  (chinese-file :unlink)
) ;let

;; 测试路径对象的read-text与path-read-text等价性
(let ((test-file (string-append (os-temp-dir) "/equiv_test.txt")))
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; 使用字符串路径设置内容
  (path-write-text test-file "Comparison test")
  
  ;; 验证path对象read-text与path-read-text结果相同
  (let ((path-obj (path test-file)))
    (check (path-obj :read-text) => (path-read-text test-file))
  ) ;let
  
  ;; 清理
  (delete-file test-file)
) ;let

;; 测试文件不存在时的错误处理
(let ((nonexistent-file (path :temp-dir :/ "not_exists.txt")))
  (when (nonexistent-file :exists?)
    (nonexistent-file :unlink)
  ) ;when
  
  ;; 验证错误抛出
  (check-catch 'file-not-found-error (nonexistent-file :read-text))
) ;let

;; 测试大文件读取
(let ((big-file (path :temp-dir :/ "large_test.txt"))
      (large-content (make-string 10000 #\a)))
  (when (big-file :exists?)
    (big-file :unlink)
  ) ;when
  
  ;; 创建大文件
  (big-file :write-text large-content)
  
  ;; 读取大文件并验证内容完整性
  (let ((read-content (big-file :read-text)))
    (check (string-length read-content) => 10000)
    (check (string=? read-content large-content) => #t)
  ) ;let
  
  ;; 清理
  (big-file :unlink)
) ;let

;; 测试跨路径分隔符兼容性
(let ((unix-file (path :temp-dir :/ "unix_style.txt")))
  (when (unix-file :exists?)
    (unix-file :unlink)
  ) ;when
  
  ;; 创建Unix风格路径文件
  (unix-file :write-text "Unix style path test")
  
  ;; 读取验证
  (check (unix-file :read-text) => "Unix style path test")
  
  ;; 清理
  (unix-file :unlink)
) ;let

;; 测试多层路径对象的读取
(let ((deep-file (path :temp-dir :/ "depth" :/ "nested" :/ "deep.txt")))
  (when (deep-file :exists?)
    (deep-file :unlink)
  ) ;when
  
  ;; 确保目录存在
  (let ((dir (path :temp-dir :/ "depth" :/ "nested")))
    (when (not (dir :exists?))
      (mkdir (dir :to-string))
    ) ;when
  ) ;let
  
  ;; 创建文件并写入内容
  (deep-file :write-text "Deeply nested file content")
  
  ;; 读取验证
  (check (deep-file :read-text) => "Deeply nested file content")
  
  ;; 清理
  (deep-file :unlink)
) ;let

(let ((file-name "中文文件名.txt")
      (file-content "你好，世界！"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  (path-write-text file-path file-content)
  (check (path-read-text file-path) => file-content)
  (delete-file file-path)
) ;let

#|
path%touch
创建或更新时间戳文件。如果文件已存在则更新其修改和访问时间戳，如果文件不存在则创建一个空文件。

语法
----
(path-instance :touch) → boolean

返回值
-----
boolean
返回 #t 表示操作成功完成。

描述
----
`path%touch` 是 `path-touch` 的面向对象版本，用于创建空文件或更新现有文件的时间戳，与Unix/Linux的`touch`命令功能相同。

行为特征
------
- **文件不存在时**：创建一个新的空文件
- **文件已存在时**：更新文件的最后修改时间和访问时间
- **目录路径**：同样支持，会更新目录的时间戳
- **路径对象**：直接对路径对象进行操作，更加直观

错误处理
------
- **权限错误**：如果无权限创建文件，会报错
- **路径无效**：如果父目录不存在，可能报错
- **磁盘空间**：磁盘空间不足时可能失败

与path-touch的关系
----------------
`path%touch` 是 `path-touch` 的面向对象版本：
- `(path-file :touch)` 等同 `(path-touch (path-file :to-string))`
- 但使用面向对象的语法更加简洁直观

跨平台行为
---------
- **Unix/Linux/macOS**：支持所有文件系统的时间戳操作
- **Windows**：支持NTFS、FAT等文件系统的时间戳操作

应用场景
--------
- 创建用于测试的空文件
- 更新文件时间戳以触发重新编译
- 确保文件存在以备后续写入
- 与文件存在性检查配合使用

注意事项
--------
1. 目标路径的父目录必须存在
2. 对于大文件操作效率高，无需读取文件内容
3. 创建的文件大小为0字节
4. 对已存在文件不会修改其内容
|#

;; 基本文件创建测试
(let ((temp-file (path :temp-dir :/ "test_touch_basic.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink)
  ) ;when
  
  ;; 测试创建新文件
  (check-true (temp-file :touch))
  (check-true (temp-file :exists?))
  (check-true (temp-file :file?))
  (check (temp-file :read-text) => "") ; 空文件
  
  ;; 清理
  (temp-file :unlink)
) ;let

;; 测试更新现有文件时间戳
(let ((temp-file (path :temp-dir :/ "test_touch_update.txt")))
  ;; 确保文件不存在
  (when (temp-file :exists?)
    (temp-file :unlink)
  ) ;when
  
  ;; 创建文件并写入内容
  (temp-file :write-text "initial content")
  (let ((original-content (temp-file :read-text)))
    (let ((original-size (string-length original-content)))
    
    ;; 更新时间戳
    (check-true (temp-file :touch))
    
    ;; 验证文件内容未改变
    (check (temp-file :read-text) => "initial content")
    (check (string-length (temp-file :read-text)) => original-size)
    
    ;; 验证文件仍然存在
    (check-true (temp-file :exists?))
    (check-true (temp-file :file?)))
  ) ;let
  
  ;; 清理
  (temp-file :unlink)
) ;let

;; 测试目录时间戳更新
(let ((temp-dir (path :temp-dir :/ "test_touch_dir")))
  ;; 确保目录存在
  (when (not (temp-dir :exists?))
    (mkdir (temp-dir :to-string))
  ) ;when
  
  ;; 测试更新目录时间戳
  (check-true (temp-dir :touch))
  (check-true (temp-dir :exists?))
  (check-true (temp-dir :dir?))
  
  ;; 清理空目录
  (when (temp-dir :exists?)
    (rmdir (temp-dir :to-string))
  ) ;when
) ;let

;; 测试多级路径文件创建
(let ((deep-file (path :temp-dir :/ "level1" :/ "level2" :/ "deep_touch.txt")))
  ;; 确保父目录存在
  (let ((parent-dir (path :temp-dir :/ "level1" :/ "level2"))
        (grandparent-dir (path :temp-dir :/ "level1")))
    (when (not (grandparent-dir :exists?))
      (mkdir (grandparent-dir :to-string))
    ) ;when
    (when (not (parent-dir :exists?))
      (mkdir (parent-dir :to-string)) ; 创建多级目录
    ) ;when
  ) ;let
  
  ;; 测试创建多级路径文件
  (check-true (deep-file :touch))
  (check-true (deep-file :exists?))
  (check (deep-file :read-text) => "")
  
  ;; 清理多级目录和文件
  (when (deep-file :exists?)
    (deep-file :unlink)
  ) ;when
  (let ((dir1 (path :temp-dir :/ "level1" :/ "level2"))
        (dir2 (path :temp-dir :/ "level1")))
    (when (dir1 :exists?) (rmdir (dir1 :to-string)))
    (when (dir2 :exists?) (rmdir (dir2 :to-string)))
  ) ;let
) ;let

;; 测试特殊文件名中的touch
(let ((special-file (path :temp-dir :/ "test-file.name with spaces&special#.txt")))
  ;; 确保文件不存在
  (when (special-file :exists?)
    (special-file :unlink)
  ) ;when
  
  ;; 测试特殊文件名创建
  (check-true (special-file :touch))
  (check-true (special-file :exists?))
  
  ;; 清理
  (special-file :unlink)
) ;let

;; 测试中文文件名创建
(let ((chinese-file (path :temp-dir :/ "触摸测试.txt")))
  ;; 确保文件不存在
  (when (chinese-file :exists?)
    (chinese-file :unlink)
  ) ;when
  
  ;; 测试中文文件名创建
  (check-true (chinese-file :touch))
  (check-true (chinese-file :exists?))
  
  ;; 清理
  (chinese-file :unlink)
) ;let


;; 测试相对路径touch
(let ((rel-file (path :./ "test_relative_touch.txt")))
  ;; 确保文件不存在
  (when (rel-file :exists?)
    (rel-file :unlink)
  ) ;when
  
  ;; 测试相对路径创建
  (check-true (rel-file :touch))
  (check-true (rel-file :exists?))
  
  ;; 清理
  (rel-file :unlink)
) ;let

;; 测试绝对路径创建
(when (or (os-linux?) (os-macos?))
  (let ((abs-file (path :/ "tmp" :/ "test_absolute_touch.txt")))
    ;; 确保文件不存在
    (when (abs-file :exists?)
      (abs-file :unlink)
    ) ;when
    
    ;; 测试绝对路径创建
    (check-true (abs-file :touch))
    (check-true (abs-file :exists?))
    
    ;; 清理
    (abs-file :unlink)
  ) ;let
) ;when

;; 测试重复touch同一文件
(let ((repeat-file (path :temp-dir :/ "test_repeat_touch.txt")))
  ;; 确保文件不存在
  (when (repeat-file :exists?)
    (repeat-file :unlink)
  ) ;when
  
  ;; 多次执行touch
  (check-true (repeat-file :touch))
  (check-true (repeat-file :touch))
  (check-true (repeat-file :touch))
  (check-true (repeat-file :exists?))
  (check (repeat-file :read-text) => "") ; 内容保持为空
  
  ;; 清理
  (repeat-file :unlink)
) ;let

;; 测试touch与证明存在性结合
(let ((exist-test-file (path :temp-dir :/ "test_exist_touch.txt")))
  ;; 确保文件不存在
  (when (exist-test-file :exists?)
    (exist-test-file :unlink)
  ) ;when
  
  ;; 初始状态检查
  (check-false (exist-test-file :exists?))
  
  ;; 使用touch确保文件存在
  (check-true (exist-test-file :touch))
  (check-true (exist-test-file :exists?))
  (check-true (exist-test-file :file?))
  
  ;; 写入内容
  (exist-test-file :write-text "test content")
  
  ;; 再次touch不影响内容
  (check-true (exist-test-file :touch))
  (check (exist-test-file :read-text) => "test content")
  
  ;; 清理
  (exist-test-file :unlink)
) ;let

(let1 test-file (string-append (os-temp-dir) (string (os-sep)) "test_touch.txt")
  ;; Ensure file doesn't exist initially
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  
  ;; Test creating new file with path object
  (let1 p (path test-file)
    (check-false (p :exists?))
    (check-true (p :touch))
    (check-true (p :exists?))
  ) ;let1
  
  ;; Clean up
  (delete-file test-file)
) ;let1

;; Test with very long path
(let ((long-name (make-string 200 #\x))
      (temp-dir (os-temp-dir)))
  (let ((p (path temp-dir :/ long-name)))
    (check-true (p :touch))
    (check-true (p :exists?))
    (p :unlink)
  ) ;let
) ;let


(when (not (os-windows?))
  (check-true (> (path-getsize "/") 0))
  (check-true (> (path-getsize "/etc/hosts") 0))
) ;when

(when (os-windows?)
  (check-true (> (path-getsize "C:") 0))
  (check-true (> (path-getsize "C:/Windows") 0))
  (check-true (> (path-getsize "C:\\Windows\\System32\\drivers\\etc\\hosts") 0))
) ;when

(check-report)

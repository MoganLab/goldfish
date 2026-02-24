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

(import (liii check))

(check-set-mode! 'report-failed)

(define test-filename "file-test-中文.txt")
(define test-filenames
  '("中文.txt"
    "測試.txt"
    "日本語.txt"
    "한글.txt"
    " ελληνικά.txt"
    "ملف.txt"))

(define (clean-filename filename)
  (lambda () (delete-file filename)))
(define clean-test-filename (clean-filename test-filename))

#|
with-output-to-file
将 thunk 的输出重定向到文件

函数签名
----
(with-output-to-file filename thunk) → unspecified

参数
----
filename : string
目标文件路径
thunk : procedure
无参数过程，在该过程执行期间当前输出端口绑定到 filename

返回值
----
unspecified
返回 thunk 的执行结果（测试中通过读取文件内容进行验证）

描述
----
`with-output-to-file` 会创建或覆盖目标文件，并在 thunk 执行期间把默认输出端口指向该文件。

行为特征
------
- 支持 Unicode 文件名
- 支持多行输出
- thunk 结束后自动恢复原输出端口
- 参数类型错误时抛出 `wrong-type-arg`

错误处理
------
- filename 必须是字符串
- thunk 必须是可调用过程

相关函数
--------
- `open-output-file`
- `call-with-output-file`
- `with-input-from-file`
|#
;; with-output-to-file

; 中文文件名，中文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda () (display "测试内容")))

      (call-with-input-file test-filename
        (lambda (port)
          (read-line port))))
    clean-test-filename) ; after
  => "测试内容")

; 中文文件名，英文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda () (display "ok")))

      (call-with-input-file test-filename
        (lambda (port)
          (read-line port))))
    clean-test-filename) ; after
  => "ok")

; 中文文件名，多行中文内容
(check
  (dynamic-wind
    #f ; before
    (lambda ()
      (with-output-to-file test-filename
        (lambda ()
          (display "第一行\n")
          (display "第二行")))

      (call-with-input-file test-filename
        (lambda (port)
          (list (read-line port)
                (read-line port)))))
    clean-test-filename) ; after
  => '("第一行" "第二行"))

; 测试文件是否确实被创建
(for-each
  (lambda (filename)
    (dynamic-wind
      #f ; before
      (lambda ()
        ; 确保测试文件还不存在
        (check (file-exists? filename) => #f)

        ; 测试文件创建
        (with-output-to-file filename
          (lambda () (display "test")))

        ; 验证文件存在
        ; NOTE: 若写入文件名时编码不对应，file-exists? 会返回 #f
        ;       如 `中文` 被直接写作文件名，由 Windows 解释为 GBK，会显示为 `涓枃`
        (check-true (file-exists? filename)))
      (clean-filename filename))) ; after
  test-filenames)

#|
load
从文件加载并执行 Scheme 代码

函数签名
----
(load filename) → any

参数
----
filename : string
待加载的 Scheme 文件路径

返回值
----
any
返回被加载文件中最后一个表达式的值

描述
----
`load` 会读取并执行文件内容，常用于模块导入和脚本执行。

行为特征
------
- 支持 Unicode 文件名
- 可执行由 `with-output-to-file` 写入的表达式
- 文件内容语义与 REPL 执行一致

错误处理
------
- 文件不存在或不可读时抛出文件相关异常
- 非字符串参数会触发类型错误

相关函数
--------
- `open-input-file`
- `call-with-input-file`
|#
;; load

(define test-content
  '(begin
     (define 测试变量 "你好，世界！")
     (define (测试函数 x) (+ x 1))
     #t))

(dynamic-wind
  (lambda () ; before
    (with-output-to-file test-filename
      (lambda () (display "(+ 21 21)"))))
  (lambda ()
    (check (load test-filename) => 42))
  clean-test-filename) ; after

; 测试文件是否确实被创建
(for-each
  (lambda (filename)
    (dynamic-wind
      #f ; before
      (lambda ()
        ; 确保测试文件还不存在
        (check (file-exists? filename) => #f)

        ; 测试文件创建
        (with-output-to-file filename
          (lambda () (display "(+ 21 21)")))

        ; 验证能够正常 load
        (check (load filename) => 42))
      (clean-filename filename))) ; after
  test-filenames)

#|
open-input-file / open-output-file
显式打开输入输出文件端口

函数签名
----
(open-input-file filename (mode "r")) → input-port
(open-output-file filename (mode "w")) → output-port

参数
----
filename : string
文件路径
mode : string
可选打开模式

返回值
----
port
返回对应的输入或输出端口

描述
----
用于手动控制端口生命周期，适合与 `close-input-port`/`close-output-port` 配合使用。

行为特征
------
- 支持显式端口关闭
- 与 `display`/`read-line` 等端口 API 协同工作
- 参数类型错误时抛出 `wrong-type-arg`

相关函数
--------
- `call-with-input-file`
- `call-with-output-file`
|#
;; open-output-file / open-input-file
(check
  (dynamic-wind
    #f
    (lambda ()
      (let ((op (open-output-file test-filename)))
        (display "abc" op)
        (close-output-port op))
      (let ((ip (open-input-file test-filename)))
        (let ((line (read-line ip)))
          (close-input-port ip)
          line)))
    clean-test-filename)
  => "abc")

#|
call-with-output-file / with-input-from-file
高阶文件 I/O 组合调用

函数签名
----
(call-with-output-file filename proc) → any
(with-input-from-file filename thunk) → any

参数
----
filename : string
文件路径
proc : procedure
接收一个端口参数的过程
thunk : procedure
无参数过程

返回值
----
any
返回 proc 或 thunk 的执行结果

描述
----
`call-with-output-file` 负责打开并传递端口给过程；`with-input-from-file` 在动态作用域内临时绑定当前输入端口。

行为特征
------
- 自动管理端口打开与关闭
- 适合简化常见文件读写流程
- 参数类型错误时抛出 `wrong-type-arg`

相关函数
--------
- `open-output-file`
- `open-input-file`
- `with-output-to-file`
|#
;; call-with-output-file / with-input-from-file
(check
  (dynamic-wind
    #f
    (lambda ()
      (call-with-output-file test-filename
        (lambda (port) (display "xyz" port)))
      (with-input-from-file test-filename
        (lambda () (read-line))))
    clean-test-filename)
  => "xyz")

#|
file-exists? / delete-file
文件存在性检查与删除

函数签名
----
(file-exists? path) → boolean
(delete-file path) → boolean | integer

参数
----
path : string
目标文件路径

返回值
----
boolean | integer
`file-exists?` 返回是否存在；`delete-file` 在本项目中由 `g_delete-file` 实现并用于删除文件

描述
----
`file-exists?` 用于判断路径是否存在，`delete-file` 用于删除已存在文件，两者经常配合做资源清理。

行为特征
------
- 支持测试前后清理文件
- 可用于断言文件创建与删除行为
- 非字符串参数触发 `type-error`

相关函数
--------
- `with-output-to-file`
- `open-output-file`
|#
;; file-exists? / delete-file
(check
  (dynamic-wind
    #f
    (lambda ()
      (with-output-to-file test-filename (lambda () (display "rm")))
      (check-true (file-exists? test-filename))
      (delete-file test-filename)
      (file-exists? test-filename))
    #f)
  => #f)

;; error cases
(check-catch 'type-error (file-exists? 123))
(check-catch 'type-error (delete-file 123))
(check-catch 'wrong-type-arg (open-input-file 123))
(check-catch 'wrong-type-arg (open-output-file 123))
(check-catch 'wrong-type-arg (call-with-input-file 123 (lambda (p) p)))
(check-catch 'wrong-type-arg (call-with-output-file 123 (lambda (p) p)))
(check-catch 'wrong-type-arg (with-input-from-file 123 (lambda () 1)))
(check-catch 'wrong-type-arg (with-output-to-file 123 (lambda () 1)))

(check-report)

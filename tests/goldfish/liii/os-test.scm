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

(import (liii check)
        (liii string)
        (liii os)
        (liii sys)
        (liii uuid)
        (scheme time)
        (liii base)
        (liii oop)
        (liii lang)
) ;import

(check-set-mode! 'report-failed)

(when (os-linux?)
  (check (os-type) => "Linux")
) ;when

(when (os-macos?)
  (check (os-type) => "Darwin")
) ;when

(when (os-windows?)
  (check (os-type) => "Windows")
) ;when

(when (not (os-windows?))
  (let ((t1 (current-second)))
    (os-call "sleep 1")
    (let ((t2 (current-second)))
      (check (>= (ceiling (- t2 t1)) 1) => #t)
    ) ;let
  ) ;let
) ;when

(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-true (access "/root" 'F_OK))
  (check-false (access "/root" 'R_OK))
  (check-false (access "/root" 'W_OK))
  (check-true (access (executable) 'X_OK))
) ;when

(check-true (putenv "TEST_VAR" "123"))       ; 设置环境变量
(check (getenv "TEST_VAR") => "123")         ; 验证设置成功
(check-true (putenv "TEST_VAR" "456"))       ; 修改环境变量
(check (getenv "TEST_VAR") => "456")         ; 验证修改成功
(check-true (unsetenv "TEST_VAR"))           ; 删除环境变量
(check (getenv "TEST_VAR") => #f)            ; 验证删除成功

(check-catch 'type-error (putenv 123 "abc")) ; key 非字符串
(check-catch 'type-error (putenv "ABC" 123)) ; value 非字符串

(check (string-null? (getenv "PATH")) => #f)
(unsetenv "PATH")
(check (getenv "PATH") => #f)
(unsetenv "home")
(check (getenv "home") => #f)
(check (getenv "home" "value does not found") => "value does not found")

(when (os-windows?)
  (check (string-starts? (os-temp-dir) "C:") => #t)
) ;when

(when (os-linux?)
  (check (os-temp-dir) => "/tmp")
) ;when

(when (not (os-windows?))
  (check-catch 'file-exists-error
    (mkdir "/tmp")
  ) ;check-catch
  (check (begin
           (let ((test_dir "/tmp/test_124"))
             (when (file-exists? test_dir)
               (rmdir "/tmp/test_124")
             ) ;when
             (mkdir "/tmp/test_124"))
           ) ;let
    => #t
  ) ;check
) ;when

(when (or (os-macos?) (os-linux?))
  ;; 测试 remove
  (let ((test-file (string-append (os-temp-dir) "/test_remove.txt")))
    ;; 创建临时文件
    (with-output-to-file test-file
      (lambda () (display "test data"))
    ) ;with-output-to-file
    ;; 验证文件存在
    (check-true (file-exists? test-file))
    ;; 删除文件
    (check-true (remove test-file))
    ;; 验证文件已删除
    (check-false (file-exists? test-file))
  ) ;let
) ;when

;; 错误测试
(check-catch 'type-error (remove 123))               ; path 非字符串
(check-catch 'file-not-found-error (remove "/nonexistent/file")) ; 文件不存在

;; 测试 remove 对目录的提示
(let ((test-dir (string-append (os-temp-dir) (string (os-sep)) "test_dir")))
  ;; 创建临时目录
  (when (not (file-exists? test-dir))
    (mkdir test-dir)
  ) ;when
  ;; 尝试删除目录，应提示使用 rmdir
  (check-catch 'value-error (remove test-dir))
  ;; 清理
  (rmdir test-dir)
  (when (file-exists? test-dir)
    (display* test-dir " failed to remove \n")
  ) ;when
) ;let

(when (not (os-windows?))
  (check (> (vector-length (listdir "/usr")) 0) => #t)
) ;when

(let* ((test-dir (string-append (os-temp-dir) (string (os-sep)) (uuid4)))
       (test-dir2 (string-append test-dir (string (os-sep))))
       (dir-a (string-append test-dir2 "a"))
       (dir-b (string-append test-dir2 "b"))
       (dir-c (string-append test-dir2 "c")))
  (mkdir test-dir)
  (mkdir dir-a)
  (mkdir dir-b)
  (mkdir dir-c)
  (let1 r (listdir test-dir)
    (check-true ($ r :contains "a"))
    (check-true ($ r :contains "b"))
    (check-true ($ r :contains "c"))
  ) ;let1
  (let1 r2 (listdir test-dir2)
    (check-true ($ r2 :contains "a"))
    (check-true ($ r2 :contains "b"))
    (check-true ($ r2 :contains "c"))
  ) ;let1
  (rmdir dir-a)
  (rmdir dir-b)
  (rmdir dir-c)
  (rmdir test-dir)
) ;let*

(when (os-windows?)
  (check (> (vector-length (listdir "C:")) 0) => #t)
) ;when

(check-false (string-null? (getcwd)))

(check-report)


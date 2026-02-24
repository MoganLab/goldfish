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

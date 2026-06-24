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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; gf format：统一格式化整个仓库的 C++（clang-format）与 Scheme（gf fmt）文件。
;; 扫描范围由项目根的 gfformat.json 决定（见 (liii goldformat-path)）。
;;
;; 用法：
;;   gf format           原地批量格式化 C++ 与 Scheme 文件
;;   gf format --check   非破坏性检查：发现任何未格式化文件则退出码 1（供 CI）

(define-library (liii goldformat)
  (import (liii base)
    (liii sys)
    (liii os)
    (liii path)
    (liii string)
    (scheme process-context)
    (liii goldformat-binary)
    (liii goldformat-path)
  ) ;import
  (export main)
  (begin

    (define (flush-output)
      (flush-output-port (current-output-port))
    ) ;define

    ;; 把文件列表写入临时文件，供 clang-format --files 批量处理。
    (define (write-file-list files)
      (let ((tmp (path->string (path-join (os-temp-dir) "goldformat-cpp-files.txt"))))
        (let ((port (open-output-file tmp)))
          (display (car files) port)
          (let loop
            ((fs (cdr files)))
            (if (null? fs)
              (begin
                (close-output-port port)
                tmp
              ) ;begin
              (begin
                (display (string-append "\n" (car fs)) port)
                (loop (cdr fs))
              ) ;begin
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (format-cpp)
      (let* ((cf (clang-format-binary)) (files (collect-all-cpp-files)))
        (if (null? files)
          (begin
            (display "No C++ files found.")
            (newline)
          ) ;begin
          (let ((list-file (write-file-list files)))
            (display (string-append "Formatting "
                       (number->string (length files))
                       " C++ files with "
                       cf
                     ) ;string-append
            ) ;display
            (newline)
            (flush-output)
            (os-call (string-append cf " -i --files=" list-file))
            (delete-file list-file)
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 格式化模式：把每个配置的 scm-dir 整体交给 gf fmt 处理。
    (define (format-scm)
      (let ((gf (executable)))
        (let loop
          ((dirs (config-scm-dirs)))
          (if (null? dirs)
            #t
            (begin
              (flush-output)
              (os-call (string-append gf " fmt " (car dirs)))
              (loop (cdr dirs))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 把字符串用单引号包成 shell 安全参数（POSIX 风格）。
    (define (shell-quote s)
      (string-append "'" (string-replace s "'" "'\\''") "'")
    ) ;define

    ;; gf fmt --dry-run 对 gfexclude.json 排除的文件会打印此行而非格式化结果。
    (define (excluded-output? s)
      (string-starts? s "Skipped (excluded):")
    ) ;define

    ;; 打印某语言检查结果：offenders 为需要格式化的文件列表。
    (define (print-offenders label offenders)
      (let ((n (length offenders)))
        (if (= n 0)
          (begin
            (display (string-append label ": OK"))
            (newline)
          ) ;begin
          (begin
            (display (string-append label ": " (number->string n) " file(s) need formatting")
            ) ;display
            (newline)
            (let loop
              ((fs offenders))
              (if (null? fs)
                #t
                (begin
                  (display (string-append "  " (car fs)))
                  (newline)
                  (loop (cdr fs))
                ) ;begin
              ) ;if
            ) ;let
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    ;; 逐文件 clang-format --dry-run --Werror，返回退出码非 0 的文件列表。
    ;; stderr（含 diff）重定向到 /dev/null 以免污染日志，靠退出码判定。
    (define (check-cpp)
      (let* ((cf (clang-format-binary)) (files (collect-all-cpp-files)))
        (if (null? files)
          (begin
            (display "No C++ files found.")
            (newline)
            '()
          ) ;begin
          (let loop
            ((fs files) (bad '()))
            (if (null? fs)
              (reverse bad)
              (let* ((f (car fs))
                     (rc (os-call (string-append "sh -c \""
                                    cf
                                    " --dry-run --Werror "
                                    (shell-quote f)
                                    " >/dev/null 2>&1\""
                                  ) ;string-append
                         ) ;os-call
                     ) ;rc
                    ) ;
                (loop (cdr fs) (if (= rc 0) bad (cons f bad)))
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 逐文件 gf fmt --dry-run，捕获输出到临时文件后与磁盘内容逐字节比较；
    ;; 排除文件（输出 Skipped (excluded):）或命令失败时视为通过。
    ;; 用逐文件方式是因为 goldfish 的 gf fmt --dry-run 不支持目录路径。
    (define (check-scm)
      (let* ((gf (executable))
             (files (collect-all-scm-files))
             (tmp (path->string (path-join (os-temp-dir) "goldformat-scm-check.txt")))
            ) ;
        (if (null? files)
          '()
          (let loop
            ((fs files) (bad '()))
            (if (null? fs)
              (begin
                (if (file-exists? tmp) (delete-file tmp) #f)
                (reverse bad)
              ) ;begin
              (let* ((f (car fs))
                     (rc (os-call (string-append "sh -c \""
                                    gf
                                    " fmt --dry-run "
                                    (shell-quote f)
                                    " > "
                                    tmp
                                    " 2>/dev/null\""
                                  ) ;string-append
                         ) ;os-call
                     ) ;rc
                     (captured (if (file-exists? tmp) (path-read-text tmp) ""))
                     (ondisk (path-read-text f))
                    ) ;
                (loop (cdr fs)
                  (cond ((not (= rc 0)) bad)
                        ((excluded-output? captured) bad)
                        ((string=? ondisk captured) bad)
                        (else (cons f bad))
                  ) ;cond
                ) ;loop
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 非破坏性检查：发现任意未格式化文件则退出码 1。
    ;; Windows 无 sh，本地运行跳过并退出 0（CI 在 Debian 跑）。
    (define (run-check)
      (if (os-windows?)
        (begin
          (display "format --check: skipped on Windows (requires sh).")
          (newline)
          (exit 0)
        ) ;begin
        (let* ((cpp-bad (begin
                          (display "=== Checking C++ files ===")
                          (newline)
                          (flush-output)
                          (check-cpp)
                        ) ;begin
               ) ;cpp-bad
               (scm-bad (begin
                          (display "=== Checking Scheme files ===")
                          (newline)
                          (flush-output)
                          (check-scm)
                        ) ;begin
               ) ;scm-bad
               (total (+ (length cpp-bad) (length scm-bad)))
              ) ;
          (newline)
          (print-offenders "C++" cpp-bad)
          (print-offenders "Scheme" scm-bad)
          (newline)
          (if (> total 0)
            (begin
              (display (string-append "FAIL: " (number->string total) " file(s) need formatting")
              ) ;display
              (newline)
              (exit 1)
            ) ;begin
            (begin
              (display "OK: all files formatted.")
              (newline)
              (exit 0)
            ) ;begin
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (main)
      (let ((check-mode? (member "--check" (cddr (command-line)))))
        (if check-mode?
          (run-check)
          (begin
            (display "=== Formatting C++ files ===")
            (newline)
            (flush-output)
            (format-cpp)
            (newline)
            (display "=== Formatting Scheme files ===")
            (newline)
            (flush-output)
            (format-scm)
            (newline)
            (display "Done.")
            (newline)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

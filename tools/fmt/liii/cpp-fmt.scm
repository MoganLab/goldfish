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

;; C++ 语言处理器：(liii cpp-fmt)。
;; 格式化通过外部 clang-format 完成（-i 原地改；--dry-run --Werror 做检查）。
;; 文件收集复用 (liii goldfmt-lang) 的 collect-files + glob 排除。
;; 加载时通过 register-lang! 注册进 (liii goldfmt-lang)。

(define-library (liii cpp-fmt)
  (import (liii base)
    (liii sys)
    (liii os)
    (liii path)
    (liii string)
    (liii goldfmt-lang)
  ) ;import
  (export clang-format-binary
    collect-cpp
    format-cpp-files
    check-cpp-file
    cpp-handler
  ) ;export
  (begin

    ;; ---- clang-format 定位（迁移自 goldformat-binary.scm）--------------
    ;; Windows 直接用 PATH 中的 clang-format；macOS 用 homebrew llvm@19；
    ;; Linux 按版本号优先探测 clang-format-19，回退到通用名。
    (define (clang-format-binary)
      (cond ((os-windows?) "clang-format")
            ((os-macos?) "/opt/homebrew/opt/llvm@19/bin/clang-format")
            (else (let loop
                    ((paths '("/usr/local/bin/clang-format-19"
                              "/usr/lib/llvm-19/bin/clang-format"
                              "/usr/bin/clang-format-19"
                              "/usr/bin/clang-format")
                     ) ;paths
                    ) ;
                    (if (null? paths)
                      "clang-format"
                      (if (file-exists? (car paths)) (car paths) (loop (cdr paths)))
                    ) ;if
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 把字符串用单引号包成 shell 安全参数（POSIX 风格）。
    (define (shell-quote s)
      (string-append "'" (string-replace s "'" "'\\''") "'")
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

    ;; ---- 文件收集 -------------------------------------------------------
    ;; 按配置 path 收集所有 C/C++ 文件（suffixes 由调用方传）。
    (define (collect-cpp paths suffixes excludes)
      (let loop
        ((ps paths) (acc '()))
        (if (null? ps)
          acc
          (if (path-dir? (path (car ps)))
            (loop (cdr ps) (append (collect-files (car ps) suffixes excludes) acc))
            (loop (cdr ps) acc)
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 批量格式化 -----------------------------------------------------
    ;; files 为空时打印提示；否则写文件列表后 clang-format -i --files 批量改。
    ;; 返回处理文件数。
    (define (format-cpp-files files)
      (if (null? files)
        (begin
          (display "No C++ files found.")
          (newline)
          0
        ) ;begin
        (let* ((cf (clang-format-binary)) (list-file (write-file-list files)))
          (display (string-append "Formatting "
                     (number->string (length files))
                     " C++ files with "
                     cf
                   ) ;string-append
          ) ;display
          (newline)
          (flush-output-port (current-output-port))
          (os-call (string-append cf " -i --files=" list-file))
          (delete-file list-file)
          (length files)
        ) ;let*
      ) ;if
    ) ;define

    ;; ---- 单文件检查 -----------------------------------------------------
    ;; clang-format --dry-run --Werror，退出码非 0 表示需格式化。
    ;; stderr（含 diff）重定向到 /dev/null；返回 #t(已格式化) / #f(需格式化)。
    ;; Windows 无 sh：视为通过（与原 gf format --check 一致，CI 在 Debian 跑）。
    (define (check-cpp-file path-str)
      (if (os-windows?)
        #t
        (let* ((cf (clang-format-binary))
               (rc (os-call (string-append "sh -c \""
                              cf
                              " --dry-run --Werror "
                              (shell-quote path-str)
                              " >/dev/null 2>&1\""
                            ) ;string-append
                   ) ;os-call
               ) ;rc
              ) ;
          (= rc 0)
        ) ;let*
      ) ;if
    ) ;define

    ;; ---- handler 协议 ---------------------------------------------------
    ;; cpp 的 collect 需要后缀，故 handler 暴露一个接收 suffixes 的版本；
    ;; 主入口在仓库批量/check 时直接调用 collect-cpp/format-cpp-files/check-cpp-file。
    (define cpp-handler
      (list (cons 'name 'cpp)
        (cons 'collect collect-cpp)
        (cons 'check-file (lambda (path excludes) (check-cpp-file path)))
        (cons 'format-files (lambda (files dry-run excludes) (format-cpp-files files)))
      ) ;list
    ) ;define

    (register-lang! cpp-handler)

  ) ;begin
) ;define-library

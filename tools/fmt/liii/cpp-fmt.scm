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
    (liii goldfmt-config)
  ) ;import
  (export clang-format-binary
    cpp-extensions
    format-cpp-file
    format-cpp-files
    format-cpp-directory
    check-cpp-file
  ) ;export
  (begin

    ;; C++ 语言接管的后缀表（带点）。gf_fmt.json 未写 cpp.suffix 时也用此表。
    (define cpp-extensions '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))

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

    ;; 单文件格式化：dry-run 时输出 clang-format 的 dry-run 结果；
    ;; 否则把格式化结果输出到临时文件与原内容比对——不同才写回（计 updated）。
    (define (format-cpp-file path-str dry-run)
      (let ((cf (clang-format-binary)))
        (if dry-run
          (os-call (string-append cf " --dry-run " (shell-quote path-str)))
          (let* ((tmp (path->string (path-join (os-temp-dir) "goldformat-cpp-out.txt")))
                 (rc (os-call (string-append "sh -c \"" cf " " (shell-quote path-str) " > " tmp "\"")
                     ) ;os-call
                 ) ;rc
                 (formatted (if (file-exists? tmp) (path-read-text (path tmp)) ""))
                 (ondisk (path-read-text (path path-str)))
                ) ;
            (if (file-exists? tmp) (delete-file tmp) #f)
            (if (and (= rc 0) (not (string=? formatted ondisk)))
              (begin
                (path-write-text (path path-str) formatted)
                (display (string-append "  Updated: " path-str))
                (newline)
              ) ;begin
              (begin
                (display (string-append "  Unchanged: " path-str))
                (newline)
              ) ;begin
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 文件收集 -------------------------------------------------------
    ;; 仓库批量收集：从 cfg 的 cpp.path 递归收集 C/C++ 文件（按 cpp.suffix，尊重 cpp.exclude）。
    (define (cpp-collect cfg)
      (let ((paths (lang-paths 'cpp cfg))
            (suffixes (lang-suffixes 'cpp cfg))
            (excludes (lang-excludes 'cpp cfg))
           ) ;
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
      ) ;let
    ) ;define

    ;; ---- 批量格式化 -----------------------------------------------------
    ;; 逐文件 clang-format：把格式化结果输出到临时文件，与原内容比对——
    ;; 不同则写回（计 updated），相同则跳过（计 unchanged）。返回 (total updated unchanged)。
    ;; 无缓存（cpp 不缓存），统计诚实反映真实改动。cfg 参数为统一协议保留。
    (define (format-one-cpp cf path-str tmp)
      (let* ((rc (os-call (string-append "sh -c \"" cf " " (shell-quote path-str) " > " tmp "\"")
                 ) ;os-call
             ) ;rc
             (formatted (if (file-exists? tmp) (path-read-text (path tmp)) ""))
             (ondisk (path-read-text (path path-str)))
            ) ;
        (if (and (= rc 0) (not (string=? formatted ondisk)))
          (begin
            (path-write-text (path path-str) formatted)
            (display (string-append "  Updated: " path-str))
            (newline)
            #t
          ) ;begin
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (format-cpp-files files cfg)
      (if (null? files)
        (begin
          (display "No C++ files found.")
          (newline)
          (list 0 0 0)
        ) ;begin
        (let* ((cf (clang-format-binary))
               (tmp (path->string (path-join (os-temp-dir) "goldformat-cpp-out.txt")))
              ) ;
          (display (string-append "Formatting "
                     (number->string (length files))
                     " C++ files with "
                     cf
                   ) ;string-append
          ) ;display
          (newline)
          (flush-output-port (current-output-port))
          (let loop
            ((fs files) (total 0) (updated 0))
            (if (null? fs)
              (begin
                (if (file-exists? tmp) (delete-file tmp) #f)
                (list total updated (- total updated))
              ) ;begin
              (let ((changed? (format-one-cpp cf (car fs) tmp)))
                (loop (cdr fs) (+ total 1) (if changed? (+ updated 1) updated))
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    ;; 目录递归格式化：收集 dir 下命中 suffixes 的 C/C++ 文件（尊重 excludes），
    ;; 逐文件 clang-format 比对内容（同 format-cpp-files）。返回 (total updated unchanged)。
    ;; dry-run 不支持目录（与 scheme 目录约定一致，由调用方拦截）。
    (define (format-cpp-directory dir suffixes excludes)
      (let ((files (collect-files dir suffixes excludes)))
        (if (null? files)
          (begin
            (display "No C++ files found.")
            (newline)
            (list 0 0 0)
          ) ;begin
          (format-cpp-files files #f)
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 单文件检查 -----------------------------------------------------
    ;; clang-format --dry-run --Werror，退出码非 0 表示需格式化。
    ;; stderr（含 diff）重定向到 /dev/null；返回 #t(已格式化) / #f(需格式化)。
    ;; Windows 无 sh：视为通过（CI 在 Debian 跑）。cfg 参数为统一协议保留。
    (define (check-cpp-file path cfg)
      (if (os-windows?)
        #t
        (let* ((cf (clang-format-binary))
               (rc (os-call (string-append "sh -c \""
                              cf
                              " --dry-run --Werror "
                              (shell-quote path)
                              " >/dev/null 2>&1\""
                            ) ;string-append
                   ) ;os-call
               ) ;rc
              ) ;
          (= rc 0)
        ) ;let*
      ) ;if
    ) ;define

    ;; ---- 注册到语言注册表 -----------------------------------------------
    ;; format-file / format-directory 用 lambda 适配到统一协议签名
    ;; （cpp 单文件不读 excludes；cpp 目录不支持 dry-run，由主入口拦截）。
    (define (cpp-format-file path dry-run excludes)
      (format-cpp-file path dry-run)
    ) ;define

    (define (cpp-format-directory dir exts excludes dry-run)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (format-cpp-directory dir exts excludes)
      ) ;if
    ) ;define

    (define cpp-handler
      (list (cons 'name 'cpp)
        (cons 'label "C++")
        (cons 'extensions cpp-extensions)
        (cons 'collect cpp-collect)
        (cons 'format-files format-cpp-files)
        (cons 'format-file cpp-format-file)
        (cons 'format-directory cpp-format-directory)
        (cons 'check-file check-cpp-file)
      ) ;list
    ) ;define

    (register-lang! cpp-handler)

  ) ;begin
) ;define-library

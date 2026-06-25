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

;; gf fmt 的多语言配置加载（gf_fmt.json）。
;; 该文件三合一承载了原 gfformat.json（扫描范围）与 gfexclude.json（排除）：
;;   {
;;     "cpp":    { "suffix": ["hpp","cpp","h","c","cc","cxx"], "path": ["src"],
;;                 "exclude": [{"path":"src/s7*","reason":"..."}, "tests"] },
;;     "scheme": { "suffix": ["scm"], "path": ["goldfish","tools"],
;;                 "exclude": ["tests"] }
;;   }
;; 字段语义：
;;   suffix  —— 后缀字符串数组（不带点），如 ["hpp","cpp"]。
;;   path    —— 根目录/文件字符串数组，仓库批量模式时递归收集。
;;   exclude —— 该语言专属排除，支持两种格式：
;;                纯字符串 "src/s7*" 或对象 {"path":"...","reason":"..."}。
;;              支持通配符 *（匹配逻辑在各语言模块/通用层）。
;; 配置文件放在项目根（g_project-root，即 gfproject.json 所在目录）。

(define-library (liii goldfmt-config)
  (import (liii base) (liii os) (liii path) (liii string) (liii json))
  (export load-fmt-config
    config-exists?
    lang-suffixes
    lang-paths
    lang-excludes
    lang-binary
    default-suffixes
    exclude-entry->path
    parse-exclude-array
  ) ;export
  (begin

    ;; 各语言的后缀默认值：未在配置里写 suffix 时使用。
    (define (default-suffixes lang)
      (cond ((eq? lang 'cpp) '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))
            ((eq? lang 'scheme) '(".scm"))
            (else '())
      ) ;cond
    ) ;define

    ;; 逗号分隔字符串切分成后缀列表（不带点的补成带点，丢弃空串）。
    (define (normalize-suffix ext)
      (if (and (> (string-length ext) 0) (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext)
      ) ;if
    ) ;define

    ;; 从 JSON 值里读字符串列表：仅接受字符串数组，逐项收集字符串、丢弃非字符串。
    ;; 非数组或空则返回 '()。
    (define (read-string-array val)
      (if (not (json-array? val))
        '()
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length val))
            (reverse acc)
            (let ((p (vector-ref val i)))
              (loop (+ i 1) (if (string? p) (cons p acc) acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 从配置里读某语言的后缀列表（带点）。配置未写或为空则用默认。
    (define (lang-suffixes lang cfg)
      (let ((arr (read-string-array (json-ref (json-ref cfg (symbol->string lang)) "suffix"))
            ) ;arr
           ) ;
        (if (null? arr) (default-suffixes lang) (map normalize-suffix arr))
      ) ;let
    ) ;define

    ;; 从配置里读某语言的扫描 path 列表。
    (define (lang-paths lang cfg)
      (read-string-array (json-ref (json-ref cfg (symbol->string lang)) "path"))
    ) ;define

    ;; exclude 数组每项可以是字符串（纯路径）或对象（含 path 与可选 reason）。
    (define (exclude-entry->path entry)
      (if (string? entry) entry (json-ref-string entry "path" #f))
    ) ;define

    ;; 纯解析：从 JSON 数组节点构造 exclude pattern 字符串列表。
    ;; 非数组或空则返回 '()。对象项取 "path"，非字符串则丢弃。
    (define (parse-exclude-array arr)
      (if (not (json-array? arr))
        '()
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length arr))
            (reverse acc)
            (let ((p (exclude-entry->path (vector-ref arr i))))
              (loop (+ i 1) (if (string? p) (cons p acc) acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 从配置里读某语言的 exclude pattern 列表。
    (define (lang-excludes lang cfg)
      (parse-exclude-array (json-ref (json-ref cfg (symbol->string lang)) "exclude"))
    ) ;define

    ;; 从配置里读取某语言的外部格式化器二进制路径，支持按操作系统覆盖：
    ;;   "binary"            : 默认路径
    ;;   "binary-linux"      : Linux 覆盖
    ;;   "binary-windows"    : Windows 覆盖
    ;;   "binary-macos"      : macOS 覆盖
    ;; 未配置时返回默认字符串 "clang-format"。
    (define (lang-binary lang cfg)
      (let* ((lang-cfg (json-ref cfg (symbol->string lang)))
             (os-key (cond ((os-windows?) "binary-windows")
                           ((os-macos?) "binary-macos")
                           (else "binary-linux")
                     ) ;cond
             ) ;os-key
             (os-bin (json-ref-string lang-cfg os-key #f))
             (generic-bin (json-ref-string lang-cfg "binary" #f))
            ) ;
        (or os-bin generic-bin "clang-format")
      ) ;let*
    ) ;define

    ;; 项目根下是否存在 gf_fmt.json。
    (define (config-exists?)
      (let ((root (g_project-root)))
        (and root
          (string? root)
          (not (string=? root ""))
          (file-exists? (string-append root "/gf_fmt.json"))
        ) ;and
      ) ;let
    ) ;define

    ;; 读取并解析项目根下的 gf_fmt.json，返回 JSON 对象。
    ;; 不存在或无项目根时返回 #f；解析失败抛异常（由调用方 catch）。
    (define (load-fmt-config)
      (let ((root (g_project-root)))
        (if (or (not root) (not (string? root)) (string=? root ""))
          #f
          (let ((file (string-append root "/gf_fmt.json")))
            (if (not (file-exists? file)) #f (string->json (path-read-text (path file))))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

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
  (import (liii base) (liii os) (liii path) (liii string) (liii json) (liii list))
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

    ;; 后缀归一化：已带点前缀的原样返回，否则补点；空串返回 #f（丢弃）。
    (define (normalize-suffix ext)
      (cond ((or (not (string? ext)) (string=? ext "")) #f)
            ((char=? (string-ref ext 0) #\.) ext)
            (else (string-append "." ext))
      ) ;cond
    ) ;define

    ;; 从 JSON 值里读字符串列表：仅接受字符串数组，逐项收集字符串、丢弃非字符串。
    ;; 非数组或空则返回 '()。
    (define (read-string-array val)
      (if (json-array? val) (filter string? (vector->list val)) '())
    ) ;define

    ;; 从配置里读某语言的后缀列表（带点）。配置未写或为空则用默认；空串项被丢弃。
    (define (lang-suffixes lang cfg)
      (let ((arr (read-string-array (json-ref (json-ref cfg (symbol->string lang)) "suffix"))
            ) ;arr
           ) ;
        (if (null? arr)
          (default-suffixes lang)
          (filter identity (map normalize-suffix arr))
        ) ;if
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
      (if (json-array? arr)
        (filter identity (map exclude-entry->path (vector->list arr)))
        '()
      ) ;if
    ) ;define

    ;; 从配置里读某语言的 exclude pattern 列表。
    (define (lang-excludes lang cfg)
      (parse-exclude-array (json-ref (json-ref cfg (symbol->string lang)) "exclude"))
    ) ;define

    ;; 候选路径是否“可用”：含路径分隔符（/ 或 \）的视为显式路径，
    ;; 用 file-exists? + 可执行位（access X_OK）检查；裸命令名（如 "clang-format"）
    ;; 视为 PATH 查找，恒可用（交给子进程解析）。
    (define (binary-usable? cand)
      (and (string? cand)
        (or (not (or (string-contains? cand "/") (string-contains? cand "\\")))
          (and (file-exists? cand) (access cand 'X_OK))
        ) ;or
      ) ;and
    ) ;define

    ;; 把一个 binary 配置项（字符串或字符串数组）展开为候选路径列表。
    ;; 字符串视为单元素列表；数组整体转列表（非字符串项由 binary-usable? 过滤）。
    (define (binary-candidates val)
      (cond ((string? val) (list val))
            ((json-array? val) (vector->list val))
            (else '())
      ) ;cond
    ) ;define

    ;; 取候选列表里第一个可用的路径；都没有则 #f。
    (define (first-usable-binary candidates)
      (or (find binary-usable? candidates) #f)
    ) ;define

    ;; 从配置里读取某语言的外部格式化器二进制路径，支持按操作系统覆盖：
    ;;   "binary"            : 默认路径
    ;;   "binary-linux"      : Linux 覆盖
    ;;   "binary-windows"    : Windows 覆盖
    ;;   "binary-macos"      : macOS 覆盖
    ;; 每一项既可为字符串，也可为字符串数组（数组取第一个存在且可执行的）。
    ;; 未配置或全部不可用时返回默认字符串 "clang-format"。
    (define (lang-binary lang cfg)
      (let* ((lang-cfg (json-ref cfg (symbol->string lang)))
             (os-key (cond ((os-windows?) "binary-windows")
                           ((os-macos?) "binary-macos")
                           (else "binary-linux")
                     ) ;cond
             ) ;os-key
             (os-bin (first-usable-binary (binary-candidates (json-ref lang-cfg os-key))))
             (generic-bin (first-usable-binary (binary-candidates (json-ref lang-cfg "binary")))
             ) ;generic-bin
            ) ;
        (or os-bin generic-bin "clang-format")
      ) ;let*
    ) ;define

    ;; g_project-root 是否为有效（非空）字符串。
    (define (valid-root? root)
      (and root (string? root) (not (string=? root "")))
    ) ;define

    ;; 项目根下是否存在 gf_fmt.json。
    (define (config-exists?)
      (let ((root (g_project-root)))
        (and (valid-root? root) (file-exists? (string-append root "/gf_fmt.json")))
      ) ;let
    ) ;define

    ;; 读取并解析项目根下的 gf_fmt.json，返回 JSON 对象。
    ;; 不存在或无项目根时返回 #f；解析失败抛异常（由调用方 catch）。
    (define (load-fmt-config)
      (let ((root (g_project-root)))
        (if (not (valid-root? root))
          #f
          (let ((file (string-append root "/gf_fmt.json")))
            (if (not (file-exists? file)) #f (string->json (path-read-text (path file))))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

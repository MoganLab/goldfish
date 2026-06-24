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

;; 项目级格式化范围配置（gfformat.json）的解析与文件收集。
;; 与 gfexclude.json 同样的查找方式：放在项目根（gfproject.json 所在目录）下。
;; 文件格式：
;;   {
;;     "cpp-roots": ["src"],
;;     "scm-dirs":  ["goldfish", "tools"]
;;   }
;; cpp-roots：递归收集这些根目录下的 C/C++ 源文件交给 clang-format。
;; scm-dirs：把这些目录整体交给 gf fmt 处理（格式化模式），或递归收集其中的
;;           .scm 文件做逐文件检查（--check 模式）。
;; 测试目录（tests）通过 gfexclude.json 的文件夹排除跳过，不在此声明。

(define-library (liii goldformat-path)
  (import (liii base) (liii path) (liii string) (liii json))
  (export load-format-config
    config-cpp-roots
    config-scm-dirs
    collect-cpp-files
    collect-all-cpp-files
    collect-scm-files
    collect-all-scm-files
  ) ;export
  (begin

    ;; 把 JSON 数组节点转换成字符串列表，跳过非字符串元素。
    (define (json-array->list arr)
      (if (not (vector? arr))
        '()
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length arr))
            (reverse acc)
            (let ((item (vector-ref arr i)))
              (loop (+ i 1) (if (string? item) (cons item acc) acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 把路径分隔符统一成正斜杠，便于跨平台做后缀匹配。
    (define (normalize-sep s)
      (string-replace s "\\" "/")
    ) ;define

    ;; pattern 是否含通配符 *。
    (define (has-wildcard? s)
      (let loop
        ((i (- (string-length s) 1)))
        (if (< i 0) #f (if (char=? (string-ref s i) #\*) #t (loop (- i 1))))
      ) ;let
    ) ;define

    ;; 在 str 中从 start 起查找 sub 的首位置，找不到返回 #f。
    (define (string-index-of str sub start)
      (let ((sublen (string-length sub)) (slen (string-length str)))
        (let loop
          ((i start))
          (cond ((> (+ i sublen) slen) #f)
                ((string=? (substring str i (+ i sublen)) sub) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (string-starts-with? str prefix)
      (and (>= (string-length str) (string-length prefix))
        (string=? (substring str 0 (string-length prefix)) prefix)
      ) ;and
    ) ;define

    ;; 简单 glob 匹配：只支持 *（匹配任意字符序列，含空）。
    ;; 用法如 (glob-match? "s7*" "s7_continuation.c") => #t。
    ;; 实现：把 pattern 按 * 切成若干段，逐段在 str 中顺序匹配——
    ;; 首段须是 str 前缀，末段须是 str 后缀（当 pattern 不以 * 结尾时），中间各段按序定位。
    (define (glob-match? pattern str)
      (if (not (has-wildcard? pattern))
        (string=? pattern str)
        (let ((parts (string-split pattern "*")) (ends-with-star (string-ends? pattern "*")))
          (let loop
            ((ps parts) (pos 0) (first? #t))
            (cond ((null? ps)
                   ;; 段都消耗完；pattern 以 * 结尾则已匹配，否则要求 pos 恰好走到 str 末尾。
                   (or ends-with-star (= pos (string-length str)))
                  ) ;
                  (else (let ((seg (car ps)))
                          (cond ((string=? seg "")
                                 ;; 空段（连续 * 或首尾 *）：跳过
                                 (loop (cdr ps) pos #f)
                                ) ;
                                (first?
                                  ;; 第一段必须前缀匹配
                                  (if (string-starts-with? str seg) (loop (cdr ps) (string-length seg) #f) #f)
                                ) ;first?
                                (else
                                  ;; 在 str 的 pos 之后查找本段
                                  (let ((found (string-index-of str seg pos)))
                                    (if found (loop (cdr ps) (+ found (string-length seg)) #f) #f)
                                  ) ;let
                                ) ;else
                          ) ;cond
                        ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 判断 entry-str 是否命中单个 exclude pattern：
    ;; - pattern 含通配符 * 时按 glob 匹配。pattern 含 / 时对完整路径匹配
    ;;   （如 "src/s7*" 匹配 src/s7.c、src/s7_continuation.c），否则对 basename 匹配
    ;;   （如 "test*" 匹配任意目录下的 test 开头文件/目录名）。
    ;; - pattern 不含通配符时按精确路径匹配：entry 等于 pattern、或 entry 以 /pattern 结尾
    ;;   （如 "tests" 匹配 tools/fmt/tests、tests）。
    (define (path-matches-exclude? entry-str pattern)
      (let ((entry-norm (normalize-sep entry-str)) (p (normalize-sep pattern)))
        (if (has-wildcard? p)
          (glob-match? p
            (if (string-contains? p "/") entry-norm (path-name (path entry-norm)))
          ) ;glob-match?
          (let ((plen (string-length p)) (elen (string-length entry-norm)))
            (or (string=? entry-norm p)
              (and (>= elen (+ plen 1))
                (char=? (string-ref entry-norm (- elen plen 1)) #\/)
                (string-ends? entry-norm p)
              ) ;and
            ) ;or
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (file-excluded? entry-str excludes)
      (let loop
        ((pats excludes))
        (if (null? pats)
          #f
          (if (path-matches-exclude? entry-str (car pats)) #t (loop (cdr pats)))
        ) ;if
      ) ;let
    ) ;define

    ;; 读取项目根下的 gfexclude.json，返回 exclude pattern 字符串列表。
    ;; 与 gf fmt / gf fix 共享同一份排除配置：C++ 和 Scheme 收集都尊重它。
    ;; 不存在或不可解析时返回 '()（静默降级，gf fmt 也是这行为）。
    (define (read-project-excludes)
      (let ((root (g_project-root)))
        (if (or (not root) (not (string? root)) (string=? root ""))
          '()
          (let ((file (string-append root "/gfexclude.json")))
            (if (not (file-exists? file))
              '()
              (catch #t
                (lambda ()
                  (let* ((obj (string->json (path-read-text (path file))))
                         (arr (json-ref obj "exclude"))
                        ) ;
                    (if (not (vector? arr))
                      '()
                      (let loop
                        ((i 0) (acc '()))
                        (if (>= i (vector-length arr))
                          (reverse acc)
                          (let ((entry (vector-ref arr i)))
                            (let ((p (if (string? entry) entry (json-ref entry "path"))))
                              (loop (+ i 1) (if (string? p) (cons p acc) acc))
                            ) ;let
                          ) ;let
                        ) ;if
                      ) ;let
                    ) ;if
                  ) ;let*
                ) ;lambda
                (lambda (type info) '())
              ) ;catch
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 读取并解析项目根下的 gfformat.json，返回 (cpp-roots . scm-dirs)。
    ;; 不存在或不可解析时报错退出：gf format 必须显式声明扫描范围，
    ;; 以免误格式化无关目录（这是让该命令可跨项目复用的前提）。
    (define (load-format-config)
      (let ((root (g_project-root)))
        (if (or (not root) (not (string? root)) (string=? root ""))
          (begin
            (display "error: 无法定位项目根（缺少 gfproject.json）。")
            (newline)
            (exit 1)
          ) ;begin
          (let ((file (string-append root "/gfformat.json")))
            (if (not (file-exists? file))
              (begin
                (display "error: 找不到 gfformat.json，请在项目根创建它，例如："
                ) ;display
                (newline)
                (display "  { \"cpp-roots\": [\"src\"],")
                (newline)
                (display "    \"scm-dirs\": [\"goldfish\", \"tools\"] }")
                (newline)
                (exit 1)
              ) ;begin
              (catch #t
                (lambda ()
                  (let* ((obj (string->json (path-read-text (path file))))
                         (cpp (json-array->list (json-ref obj "cpp-roots")))
                         (scm (json-array->list (json-ref obj "scm-dirs")))
                        ) ;
                    (cons cpp scm)
                  ) ;let*
                ) ;lambda
                (lambda (type info)
                  (let ((e (if (null? info) type (car info))))
                    (display (string-append "error: gfformat.json 解析失败 - "
                               (if (string? e) e (object->string e))
                             ) ;string-append
                    ) ;display
                    (newline)
                    (exit 1)
                  ) ;let
                ) ;lambda
              ) ;catch
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (config-cpp-roots)
      (car (load-format-config))
    ) ;define

    (define (config-scm-dirs)
      (cdr (load-format-config))
    ) ;define

    (define cpp-exts '(".cpp" ".hpp" ".h" ".c" ".cc" ".cxx"))

    (define (cpp-file? name)
      (let loop
        ((exts cpp-exts))
        (if (null? exts) #f (if (string-ends? name (car exts)) #t (loop (cdr exts))))
      ) ;let
    ) ;define

    ;; 递归收集 dir-path 下的 C/C++ 源文件（按 path-list-path 返回的相对路径原样）。
    ;; excludes（gfexclude.json）对文件和子目录都生效；命中则跳过文件或整棵子树。
    (define (collect-cpp-files dir-path excludes)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length entries))
            acc
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((s (path->string entry)))
                       (if (and (cpp-file? s) (not (file-excluded? s excludes)))
                         (loop (+ i 1) (cons s acc))
                         (loop (+ i 1) acc)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (let ((d (path->string entry)))
                       (if (file-excluded? d excludes)
                         (loop (+ i 1) acc)
                         (loop (+ i 1) (append (collect-cpp-files d excludes) acc))
                       ) ;if
                     ) ;let
                    ) ;
                    (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 遍历配置的所有 cpp-roots，合并收集到的 C/C++ 文件列表。
    ;; 收集前读取 gfexclude.json，跳过被排除的文件/目录。
    (define (collect-all-cpp-files)
      (let ((excludes (read-project-excludes)))
        (let loop
          ((roots (config-cpp-roots)) (acc '()))
          (if (null? roots)
            acc
            (if (path-dir? (path (car roots)))
              (loop (cdr roots) (append acc (collect-cpp-files (car roots) excludes)))
              (loop (cdr roots) acc)
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (scm-file? name)
      (string-ends? name ".scm")
    ) ;define

    ;; 递归收集 dir-path 下的 .scm 文件。
    ;; excludes（gfexclude.json）对文件和子目录都生效；命中则跳过文件或整棵子树。
    (define (collect-scm-files dir-path excludes)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (acc '()))
          (if (>= i (vector-length entries))
            acc
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((s (path->string entry)))
                       (if (and (scm-file? s) (not (file-excluded? s excludes)))
                         (loop (+ i 1) (cons s acc))
                         (loop (+ i 1) acc)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (let ((d (path->string entry)))
                       (if (file-excluded? d excludes)
                         (loop (+ i 1) acc)
                         (loop (+ i 1) (append (collect-scm-files d excludes) acc))
                       ) ;if
                     ) ;let
                    ) ;
                    (else (loop (+ i 1) acc))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 遍历配置的所有 scm-dirs，合并收集到的 .scm 文件列表。
    ;; 收集前读取 gfexclude.json，跳过被排除的文件/目录。
    (define (collect-all-scm-files)
      (let ((excludes (read-project-excludes)))
        (let loop
          ((dirs (config-scm-dirs)) (acc '()))
          (if (null? dirs)
            acc
            (if (path-dir? (path (car dirs)))
              (loop (cdr dirs) (append acc (collect-scm-files (car dirs) excludes)))
              (loop (cdr dirs) acc)
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii goldtest-cache)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii os)
    (liii path)
    (liii sys)
    (liii list)
    (liii string)
    (liii sort)
    (liii hashlib)
    (liii goldtool cache)
  ) ;import
  (export tools-test-path? extract-file-imports resolve-tool-import-path
    find-tools-dependencies compute-tools-deps-hash compute-test-action-key
    test-action-key test-cache-base-dir test-cache-path test-cache-hit?
    test-cache-touch! test-cache-record! test-cache-clear!
  ) ;export
  (begin

    ;; 判断给定测试文件路径是否属于 tools/ 目录
    (define (tools-test-path? path)
      (cond ((not (string? path)) #f)
            ((string-starts? path "tools/") #t)
            ((or (string-starts? path "library/")
               (string-starts? path "goldfish/")
               (string-starts? path "r7rs/")
             ) ;or
             #f
            ) ;
            (else
              (let ((cwd (getcwd)))
                (cond ((or (string-contains cwd "/tools/")
                         (string-contains cwd "\\tools\\")
                         (string-ends? cwd "/tools")
                         (string-ends? cwd "\\tools")
                       ) ;or
                       #t
                      ) ;
                      ((or (string-contains path "/tools/") (string-contains path "\\tools\\")) #t)
                      (else #f)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 解包 R7RS 导入集修饰符（only, except, prefix, rename）
    (define (%unwrap-import-set set)
      (cond ((not (pair? set)) #f)
            ((memq (car set) '(only except prefix rename)) (%unwrap-import-set (cadr set)))
            ((and (pair? set) (symbol? (car set))) set)
            (else #f)
      ) ;cond
    ) ;define

    ;; 从单条顶层 AST 提取导入库名列表
    (define (%form-imported-libs form)
      (cond
       ((and (pair? form) (eq? (car form) 'import))
        (filter (lambda (x) x) (map %unwrap-import-set (cdr form)))
       ) ;
       ((and (pair? form) (eq? (car form) 'define-library))
        (let loop
          ((clauses (cddr form)) (acc '()))
          (cond ((null? clauses) (reverse acc))
                ((and (pair? (car clauses)) (eq? (caar clauses) 'import))
                 (loop (cdr clauses)
                   (append
                     (reverse (filter (lambda (x) x) (map %unwrap-import-set (cdar clauses))))
                     acc
                   ) ;append
                 ) ;loop
                ) ;
                (else (loop (cdr clauses) acc))
          ) ;cond
        ) ;let
       ) ;
       (else '())
      ) ;cond
    ) ;define

    ;; 从文件中提取所有 (import ...) 库名列表
    (define (extract-file-imports file-path)
      (if (not (file-exists? file-path))
        '()
        (catch #t
          (lambda ()
            (call-with-input-file file-path
              (lambda (p)
                (let loop
                  ((f (read p)) (acc '()))
                  (if (eof-object? f)
                    (reverse acc)
                    (loop (read p) (append (reverse (%form-imported-libs f)) acc))
                  ) ;if
                ) ;let
              ) ;lambda
            ) ;call-with-input-file
          ) ;lambda
          (lambda args '())
        ) ;catch
      ) ;if
    ) ;define

    (define (%lib-name->rel-path lib-name)
      (string-append (string-join (map symbol->string lib-name) "/") ".scm")
    ) ;define

    ;; 将库名转换为 tools 内部源文件路径（若非 tools 库则返回 #f）
    (define (resolve-tool-import-path lib-name current-file)
      (let ((rel (%lib-name->rel-path lib-name)))
        (cond
         ((file-exists? rel)
          (let ((cwd (getcwd)))
            (if (or (string-contains cwd "/tools/") (string-contains cwd "\\tools\\"))
              rel
              #f
            ) ;if
          ) ;let
         ) ;
         ((and (string-starts? current-file "tools/")
            (let* ((parts (string-split current-file "/"))
                   (tool-dir (string-append (car parts) "/" (cadr parts)))
                   (cand (string-append tool-dir "/" rel))
                  ) ;
              (and (file-exists? cand) cand)
            ) ;let*
          ) ;and
          (let* ((parts (string-split current-file "/"))
                 (tool-dir (string-append (car parts) "/" (cadr parts)))
                ) ;
            (string-append tool-dir "/" rel)
          ) ;let*
         ) ;
         (else
           (let loop
             ((tools '("pi" "test" "fmt" "fix" "check" "doc" "help" "pr" "repl"
                       "source" "version")
              ) ;tools
             ) ;
             (if (null? tools)
               #f
               (let ((cand (string-append "tools/" (car tools) "/" rel)))
                 (if (file-exists? cand) cand (loop (cdr tools)))
               ) ;let
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 递归解析测试文件及其依赖中的 tools 内部 .scm 文件路径，按字母序返回
    (define (find-tools-dependencies test-file)
      (let ((visited '()))
        (define (visit file)
          (let ((imports (extract-file-imports file)))
            (for-each
              (lambda (lib)
                (let ((dep-path (resolve-tool-import-path lib file)))
                  (when (and dep-path (not (member dep-path visited string=?)))
                    (set! visited (cons dep-path visited))
                    (visit dep-path)
                  ) ;when
                ) ;let
              ) ;lambda
              imports
            ) ;for-each
          ) ;let
        ) ;define
        (when (file-exists? test-file)
          (visit test-file)
        ) ;when
        (list-sort string<? visited)
      ) ;let
    ) ;define

    ;; 对依赖列表中的文件内容计算确定性串联指纹
    (define (compute-tools-deps-hash deps)
      (if (null? deps)
        (sha256 "")
        (let loop
          ((rest deps) (acc '()))
          (if (null? rest)
            (sha256 (apply string-append (reverse acc)))
            (let* ((f (car rest))
                   (h (if (file-exists? f) (sha256-by-file f) ""))
                   (line (string-append f "=" h "\n"))
                  ) ;
              (loop (cdr rest) (cons line acc))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 纯函数：根据四大因子计算 Action Cache Key
    (define (compute-test-action-key bin-hash test-file-hash tools-deps-hash argv-flags)
      (sha256 (string-append bin-hash ":" test-file-hash ":" tools-deps-hash ":" argv-flags)
      ) ;sha256
    ) ;define

    ;; 综合计算单测试文件的 Action Key
    (define (test-action-key test-file . maybe-flags)
      (let* ((bin-hash (if (file-exists? (executable)) (sha256-by-file (executable)) ""))
             (test-file-hash (if (file-exists? test-file) (sha256-by-file test-file) ""))
             (deps (find-tools-dependencies test-file))
             (deps-hash (compute-tools-deps-hash deps))
             (flags (if (pair? maybe-flags) (car maybe-flags) ""))
            ) ;
        (compute-test-action-key bin-hash test-file-hash deps-hash flags)
      ) ;let*
    ) ;define

    ;; 缓存根目录：~/.cache/goldfish/test/<version>/
    (define (test-cache-base-dir)
      (goldtool-cache-base-dir "test")
    ) ;define

    ;; 缓存文件路径：<base-dir>/<前2位hash>/<剩余62位hash>
    (define (test-cache-path key)
      (let ((prefix (substring key 0 2))
            (rest (substring key 2))
            (base (test-cache-base-dir))
           ) ;
        (path->string (path-join base prefix rest))
      ) ;let
    ) ;define

    ;; 检查缓存是否命中
    (define (test-cache-hit? path)
      (goldtool-cache-hit? path)
    ) ;define

    ;; 写入缓存（创建空文件）
    (define (test-cache-touch! path)
      (goldtool-cache-touch path)
    ) ;define

    ;; 根据退出码记录缓存（仅 exit-code==0 时持久化，失败绝不缓存）
    (define (test-cache-record! path exit-code)
      (if (zero? exit-code)
        (test-cache-touch! path)
        (when (file-exists? path)
          (delete-file path)
        ) ;when
      ) ;if
    ) ;define

    ;; 递归删除目录辅助函数
    (define (%rmdir-rec d)
      (when (path-dir? (path d))
        (let ((entries (path-list (path d))))
          (for-each
            (lambda (entry)
              (let ((full (path-join (path d) entry)))
                (if (path-dir? full)
                  (%rmdir-rec (path->string full))
                  (delete-file (path->string full))
                ) ;if
              ) ;let
            ) ;lambda
            (vector->list entries)
          ) ;for-each
        ) ;let
        (rmdir d)
      ) ;when
    ) ;define

    ;; 清理测试缓存目录
    (define (test-cache-clear!)
      (let ((base (test-cache-base-dir)))
        (when (path-dir? (path base))
          (%rmdir-rec base)
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

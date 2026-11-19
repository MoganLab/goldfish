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
    (liii hashlib)
    (liii hash-table)
    (liii goldtool cache)
    (liii goldtest-cache-deps)
  ) ;import
  (export tools-test-path? resolve-tool-import-path find-tools-dependencies
    compute-tools-deps-hash compute-test-action-key test-action-key
    test-cache-base-dir test-cache-path test-cache-hit? test-cache-touch!
    test-cache-record! test-cache-clear!
  ) ;export
  (begin

    ;; 判断给定测试文件路径是否属于 tools/ 目录（保留导出以兼容历史调用）
    (define (tools-test-path? path)
      (cond ((not (string? path)) #f)
            ((string-starts? path "tools/") #t)
            ((or (string-starts? path "library/")
               (string-starts? path "goldfish/")
               (string-starts? path "r7rs/")
               (string-starts? path "bignum/")
               (string-starts? path "tui/")
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

    (define (resolve-tool-import-path lib-name current-file)
      (resolve-import-path lib-name current-file)
    ) ;define

    (define (find-tools-dependencies test-file)
      (find-test-dependencies test-file)
    ) ;define

    (define *dep-file-hash-cache* (make-hash-table))

    (define (%cached-file-sha256 f)
      (if (hash-table-contains? *dep-file-hash-cache* f)
        (hash-table-ref *dep-file-hash-cache* f)
        (let ((h (if (file-exists? f) (sha256-by-file f) "")))
          (hash-table-set! *dep-file-hash-cache* f h)
          h
        ) ;let
      ) ;if
    ) ;define

    ;; 对依赖列表中的文件内容计算确定性串联指纹
    (define (compute-tools-deps-hash deps)
      (if (null? deps)
        (sha256 "")
        (let loop
          ((rest deps) (acc '()))
          (if (null? rest)
            (sha256 (apply string-append (reverse acc)))
            (let* ((f (car rest)) (h (%cached-file-sha256 f)) (line (string-append f "=" h "\n")))
              (loop (cdr rest) (cons line acc))
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 规范化测试文件绝对路径（防止不同项目同名相对路径碰撞）
    (define (%canonical-test-id test-file)
      (let ((p (path test-file)))
        (if (path-absolute? p)
          (path->string p)
          (path->string (path-join (path (getcwd)) test-file))
        ) ;if
      ) ;let
    ) ;define

    ;; 纯函数：根据四大因子计算 Action Cache Key
    (define (compute-test-action-key bin-hash test-file-hash tools-deps-hash argv-flags)
      (sha256 (string-append bin-hash ":" test-file-hash ":" tools-deps-hash ":" argv-flags)
      ) ;sha256
    ) ;define

    (define *cached-bin-hash* #f)

    (define (%get-bin-hash)
      (unless *cached-bin-hash*
        (set! *cached-bin-hash*
          (if (file-exists? (executable)) (sha256-by-file (executable)) "")
        ) ;set!
      ) ;unless
      *cached-bin-hash*
    ) ;define

    ;; 综合计算单测试文件的 Action Key
    (define (test-action-key test-file . maybe-flags)
      (let* ((bin-hash (%get-bin-hash))
             (file-hash (if (file-exists? test-file) (sha256-by-file test-file) ""))
             (test-id (%canonical-test-id test-file))
             (combined-file-hash (string-append test-id ":" file-hash))
             (deps (find-test-dependencies test-file))
             (deps-hash (compute-tools-deps-hash deps))
             (flags (if (pair? maybe-flags) (car maybe-flags) ""))
            ) ;
        (compute-test-action-key bin-hash combined-file-hash deps-hash flags)
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

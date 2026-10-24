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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii path fs)
  (export path-copy path-copy-into path-dir? path-file? path-exists?
    path-getsize path-read-text path-read-lines path-read-bytes path-write-text
    path-write-bytes path-append-text path-touch path-list path-list-path
    path-rmdir path-unlink path-rename path-mkdir
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii os)
    (liii vector)
    (liii path record)
    (liii path pure)
  ) ;import
  (begin

    ;; ; 复制文件(跨平台行为:依赖底层 g_path-copy 原语)。
    (define (path-copy source target)
      (let ((src (path->string source)) (dst (path->string target)))
        (if (not (file-exists? src))
          (file-not-found-error (string-append "No such file or directory: '" src "'"))
          (g_path-copy src dst)
        ) ;if
      ) ;let
    ) ;define

    ;; ; 复制文件到目标目录(自动取源文件名作为目标名)。
    (define (path-copy-into source target-dir)
      (let ((filename (path-name (path-from-string source))))
        (path-copy source
          (path->string (path-join (path-from-string target-dir) (path filename)))
        ) ;path-copy
      ) ;let
    ) ;define

    ;; ; 判断是否为目录。跨平台行为一致。
    (define (path-dir? p)
      (g_isdir (path->string p))
    ) ;define

    ;; ; 判断是否为普通文件。跨平台行为一致。
    (define (path-file? p)
      (g_isfile (path->string p))
    ) ;define

    ;; ; 判断路径是否存在(文件或目录)。跨平台行为一致。
    (define (path-exists? p)
      (file-exists? (path->string p))
    ) ;define

    ;; ; 获取文件大小(字节)。不存在时抛 file-not-found-error。
    (define (path-getsize p)
      (let ((s (path->string p)))
        (if (not (file-exists? s))
          (file-not-found-error (string-append "No such file or directory: '" s "'"))
          (g_path-getsize s)
        ) ;if
      ) ;let
    ) ;define

    ;; ; 读取文件为文本。不存在时抛 file-not-found-error。目录时抛 file-error。
    (define (path-read-text p)
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-text s))
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 按行读取文本文件，返回行字符串列表
    ;; ; keepends 为 #t 时保留每行末尾的换行符
    ;; ; offset 指定从第几行（0-based）开始读取
    ;; ; limit 指定最多读取的行数，#f 表示读到文件末尾
    (define* (path-read-lines p (keepends #f) (offset 0) (limit #f))
      (if (not (or (string? p) (path? p)))
        (type-error "path-read-lines: path must be string or path")
      ) ;if
      (if (not (boolean? keepends))
        (type-error "path-read-lines: keepends must be boolean")
      ) ;if
      (if (not (exact-integer? offset))
        (type-error "path-read-lines: offset must be exact integer")
      ) ;if
      (if (not (or (not limit) (exact-integer? limit)))
        (type-error "path-read-lines: limit must be exact integer or #f")
      ) ;if
      (if (< offset 0) (value-error "path-read-lines: offset must be non-negative"))
      (if (and limit (<= limit 0))
        (value-error "path-read-lines: limit must be a positive integer")
      ) ;if
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-lines s keepends offset (or limit -1)))
        ) ;cond
      ) ;let
    ) ;define*

    ;; ; 读取文件为字节向量。不存在时抛 file-not-found-error。目录时抛 file-error。
    (define (path-read-bytes p)
      (let ((s (path->string p)))
        (cond ((not (file-exists? s))
               (file-not-found-error (string-append "No such file or directory: '" s "'"))
              ) ;
              ((path-dir? s) (file-error (string-append "Is a directory: '" s "'")))
              (else (g_path-read-bytes s))
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 覆盖写入文本。content 必须为 string。
    (define (path-write-text p content)
      (if (not (string? content))
        (type-error "path-write-text: content must be string")
        (g_path-write-text (path->string p) content)
      ) ;if
    ) ;define

    ;; ; 覆盖写入字节。data 必须为 bytevector。
    (define (path-write-bytes p data)
      (if (not (byte-vector? data))
        (type-error "path-write-bytes: data must be bytevector")
        (g_path-write-bytes (path->string p) data)
      ) ;if
    ) ;define

    ;; ; 追加文本到文件末尾。
    (define (path-append-text p content)
      (if (not (string? content))
        (type-error "path-append-text: content must be string")
        (g_path-append-text (path->string p) content)
      ) ;if
    ) ;define

    ;; ; 创建空文件(若不存在),已存在则更新访问时间。
    (define (path-touch p)
      (g_path-touch (path->string p))
    ) ;define

    ;; ; 列出目录内容(字符串向量)。
    (define (path-list p)
      (listdir (path->string p))
    ) ;define

    ;; ; 列出目录内容(path 对象向量,每个元素 = base join entry)。
    (define (path-list-path p)
      (let ((base (path->string p)))
        (let ((entries (listdir base)))
          (vector-map (lambda (entry) (path-join base entry)) entries)
        ) ;let
      ) ;let
    ) ;define

    ;; ; 删除目录(必须为空)。
    (define (path-rmdir p)
      (rmdir (path->string p))
    ) ;define

    ;; ; 删除文件。missing-ok=#t 时文件不存在不报错(对齐 pathlib.Path.unlink)。
    (define* (path-unlink p (missing-ok #f))
      (let ((s (path->string p)))
        (cond ((file-exists? s) (remove s))
              (missing-ok #t)
              (else (error 'file-not-found-error (string-append "File not found: " s)))
        ) ;cond
      ) ;let
    ) ;define*

    ;; ; 重命名/移动文件或目录(对齐 pathlib.Path.rename)。
    (define (path-rename src dst)
      (rename (path->string src) (path->string dst))
    ) ;define

    ;; ; 对齐 pathlib.Path.mkdir(parents, exist_ok)
    ;; ; 差异:parents=#t 递归创建中间目录(从根向下,跳过已存在的);
    ;; ; exist_ok=#t 时已存在不报错。跨平台行为一致。
    (define* (path-mkdir p (parents #f) (exist-ok #f))
      (define (ensure-one s)
        (if (file-exists? s) #t (mkdir s))
      ) ;define
      (let ((s (path->string p)))
        (cond ((and (not exist-ok) (file-exists? s))
               (file-exists-error (string-append "File exists: '" s "'"))
              ) ;
              ((not parents)
               (let* ((path-s (path s)) (parent (path-parent path-s)))
                 (if (and (not (path=? parent path-s)) (not (path-dir? parent)))
                   (file-not-found-error (string-append "Parent directory not found: '" (path->string parent) "'")
                   ) ;file-not-found-error
                   (ensure-one s)
                 ) ;if
               ) ;let*
              ) ;
              (else
                ;; parents:从根向下逐级创建(跳过已存在的)
                (let loop
                  ((rest (reverse (vector->list (path-parents (path s))))))
                  (if (null? rest)
                    (ensure-one s)
                    (begin
                      (ensure-one (path->string (car rest)))
                      (loop (cdr rest))
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library

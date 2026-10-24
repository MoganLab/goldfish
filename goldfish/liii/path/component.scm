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

(define-library (liii path component)
  (export split-name-dots replace-last-segment path-name path-stem path-suffix
    path-suffixes path-with-name path-with-stem path-with-suffix
  ) ;export
  (import (scheme base)
    (liii base)
    (liii error)
    (liii string)
    (liii vector)
    (liii path record)
  ) ;import
  (begin

    ;; ; 解析末段的点分隔结构,返回 (values stem-list suffix-list)。
    ;; ; 隐藏文件(.bashrc)、无点、"."/".." 整体作 stem 无后缀。
    ;; ; 末尾点(foo.)当作空后缀:stem="foo", suffix=".", 对齐 Python 3.14+ pathlib。
    (define (split-name-dots name)
      (cond ((or (string=? name ".") (string=? name "..")) (values (list name) '()))
            (else
              (let ((splits (string-split name #\.)))
                (if (or (<= (length splits) 1) (string=? (car splits) ""))
                  (values (list name) '())
                  (let* ((rev (reverse splits)) (suffix-seg (car rev)) (stem-segs (reverse (cdr rev))))
                    (values stem-segs (list (string-append "." suffix-seg)))
                  ) ;let*
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 返回末段替换为 new-name 后的新 path(保留 drive/root/其他段)。
    ;; ; 差异:替换后 Windows drive/anchor 保留(如 C:\a\b.txt → C:\a\c.md),
    ;; ; posix 同理(/a/b.txt → /a/c.md)。
    ;; ; 空路径(parts 为空或仅 ".")直接返回单段 new-name 的相对路径。
    (define (replace-last-segment p new-name)
      (let* ((pp (path p))
             (parts (path-record-parts pp))
             (n (vector-length parts))
             (type (path-record-type pp))
             (drive (path-record-drive pp))
             (root (path-record-root pp))
            ) ;
        (cond
         ((or (= n 0) (and (= n 1) (string=? (vector-ref parts 0) ".")))
          (make-path-record (vector new-name) 'posix "" #f)
         ) ;
         (else
           (let ((new-parts (vector-copy parts)))
             (vector-set! new-parts (- n 1) new-name)
             (make-path-record new-parts type drive root)
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.name
    ;; ; 差异:跨平台一致 — 直接取 record parts 末段,不依赖 path->string
    ;; ; 与平台 sep。空 parts 或仅 "." 返回 ""。
    (define (path-name p)
      (let* ((pp (path p)) (parts (path-record-parts pp)) (n (vector-length parts)))
        (cond ((= n 0) "")
              ((and (= n 1) (string=? (vector-ref parts 0) ".")) "")
              (else (vector-ref parts (- n 1)))
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; 对齐 pathlib.PurePath.stem
    ;; ; 差异:基于 path-name,跨平台一致。隐藏文件(.bashrc)整体作 stem。
    (define (path-stem p)
      (let-values (((stem-segs _) (split-name-dots (path-name p))))
        (string-join stem-segs ".")
      ) ;let-values
    ) ;define

    ;; ; 对齐 pathlib.PurePath.suffix
    ;; ; 差异:基于 path-name 末段,跨平台一致。无后缀返回 ""。
    (define (path-suffix p)
      (let-values (((_ suffix-segs) (split-name-dots (path-name p))))
        (if (null? suffix-segs) "" (car suffix-segs))
      ) ;let-values
    ) ;define

    ;; ; 对齐 pathlib.PurePath.suffixes
    ;; ; 差异:基于 path-name 末段,跨平台一致。隐藏文件/无点/"."/".." 返回 #()。
    ;; ; 末尾点(foo.)返回 #("."),对齐 Python 3.14+ pathlib。
    (define (path-suffixes p)
      (let ((name (path-name p)))
        (cond ((or (string=? name ".") (string=? name "..")) #())
              (else
                (let ((splits (string-split name #\.)))
                  (if (or (<= (length splits) 1) (string=? (car splits) ""))
                    #()
                    (list->vector (map (lambda (s) (string-append "." s)) (cdr splits)))
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 对齐 pathlib.PurePath.with_name(name)
    ;; ; 差异:保留原路径的 drive/root/类型;Windows 上 C:\a\b → C:\a\new。
    (define (path-with-name p new-name)
      (if (not (string? new-name))
        (type-error "path-with-name: new-name must be string")
        (replace-last-segment p new-name)
      ) ;if
    ) ;define

    ;; ; 对齐 pathlib.PurePath.with_stem(stem)
    ;; ; 差异:保留最后一个后缀(a.tar.gz → new.gz);Windows 上保留 drive/root。
    (define (path-with-stem p new-stem)
      (if (not (string? new-stem))
        (type-error "path-with-stem: new-stem must be string")
        (let ((suffix (path-suffix p)))
          ;; 无后缀(含隐藏文件):替换整段,等价于 with-name
          (if (string-null? suffix)
            (replace-last-segment p new-stem)
            (replace-last-segment p (string-append new-stem suffix))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; ; 对齐 pathlib.PurePath.with_suffix(suffix)
    ;; ; 差异:ext="" 去后缀;ext 必须以 "." 开头;Windows 上保留 drive/root。
    (define (path-with-suffix p ext)
      (if (not (string? ext))
        (type-error "path-with-suffix: ext must be string")
        (let* ((name (path-name p)) (stem (path-stem p)))
          (cond
           ((string-null? ext)
            (if (string-null? (path-suffix (path p)))
              (path p)
              (replace-last-segment p stem)
            ) ;if
           ) ;
           ((not (char=? (string-ref ext 0) #\.))
            (value-error "path-with-suffix: ext must start with '.'")
           ) ;
           (else (replace-last-segment p (string-append stem ext)))
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

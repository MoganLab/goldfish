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

;; gf get 的来源解析：区分本地路径、仓库 URL 与 name@organization 短包名。
;; 短包名只是包身份，不含仓库地址；首次安装必须提供 URL/路径（或 --from），
;; 安装后由用户级 depends 建立短名到来源的本地映射。

(define-library (liii gfproject get source)
  (import (scheme base) (liii error) (liii os) (liii path) (liii string))
  (export get-source-kind get-source-local-path get-package-id-part)
  (begin

    ;; Windows 盘符路径，如 C:/dir 或 C:\dir
    (define (%drive-path? arg)
      (and (>= (string-length arg) 3)
        (let ((c0 (string-ref arg 0)) (c1 (string-ref arg 1)))
          (and (or (and (char>=? c0 #\a) (char<=? c0 #\z))
                 (and (char>=? c0 #\A) (char<=? c0 #\Z))
               ) ;or
            (char=? c1 #\:)
            (or (char=? (string-ref arg 2) #\/) (char=? (string-ref arg 2) #\\))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; git SCP 风格地址，如 git@host:owner/repo.git。
    ;; 与 name@organization 的区分：冒号后不是包名字符集能表达的仓库路径，
    ;; 且 host 部分含 . 或路径部分含 /。
    (define (%scp-url? arg)
      (let ((at (string-index arg #\@)) (colon (string-index arg #\:)))
        (and at
          colon
          (> colon at)
          (> at 0)
          (or (string-contains? (substring arg 0 at) ".")
            (string-contains? (substring arg (+ colon 1) (string-length arg)) "/")
          ) ;or
        ) ;and
      ) ;let
    ) ;define

    ;; (get-source-kind arg) => 'path | 'url | 'package
    (define (get-source-kind arg)
      (unless (and (string? arg) (> (string-length arg) 0))
        (type-error "get-source-kind: expected a non-empty string" arg)
      ) ;unless
      (cond ((or (string-starts? arg "~")
               (string-starts? arg "./")
               (string-starts? arg "../")
               (string-starts? arg "/")
               (string-starts? arg "\\")
               (%drive-path? arg)
             ) ;or
             'path
            ) ;
            ((or (string-contains? arg "://") (%scp-url? arg)) 'url)
            ((and (string-index arg #\@) (not (string-index arg #\/))) 'package)
            (else (value-error "gf get: cannot parse source, expected a path, repository URL, or name@organization"
                    arg
                  ) ;value-error
            ) ;else
      ) ;cond
    ) ;define

    ;; 本地路径展开：~ 展开、相对路径基于 cwd，返回归一化绝对路径字符串。
    (define (get-source-local-path arg)
      (unless (eq? (get-source-kind arg) 'path)
        (type-error "get-source-local-path: not a local path" arg)
      ) ;unless
      (let ((p (path-expanduser (path arg))))
        (path->string
          (path-resolve (if (path-absolute? p) p (path-join (getcwd) p)) #f)
        ) ;path->string
      ) ;let
    ) ;define

    ;; 校验并返回 name@organization 的一个组件（与 package 命名字符集一致）。
    (define (get-package-id-part value)
      (unless
        (and (string? value)
          (> (string-length value) 0)
          (let ((first (string-ref value 0)))
            (and (char>=? first #\a) (char<=? first #\z))
          ) ;let
          (let loop
            ((index 0))
            (if (= index (string-length value))
              #t
              (let ((ch (string-ref value index)))
                (and
                  (or (and (char>=? ch #\a) (char<=? ch #\z))
                    (and (char>=? ch #\0) (char<=? ch #\9))
                    (and (> index 0) (or (char=? ch #\-) (char=? ch #\_)))
                  ) ;or
                  (loop (+ index 1))
                ) ;and
              ) ;let
            ) ;if
          ) ;let
        ) ;and
        (value-error "gf get: package id components must match ^[a-z][a-z0-9_-]*$"
          value
        ) ;value-error
      ) ;unless
      value
    ) ;define

  ) ;begin
) ;define-library

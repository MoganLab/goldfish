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

;; 用户级 ~/.gf 存储布局：gfproject.json（depends 声明 + tools 注册）、
;; gfproject.lock（实际解析到的版本与 commit）、site-lisp（远程包源码缓存）。
;; 声明与事实分离：depends 只记录来源；版本事实在 lock 中，安装目录为
;; site-lisp/<organization>/<name>/<version>/，未指定版本时 version 为 HEAD。

(define-library (liii gfproject get store)
  (import (scheme base)
    (liii error)
    (liii json)
    (liii path)
    (liii string)
    (liii gfproject config)
    (liii gfproject package)
    (liii gfproject get source)
  ) ;import
  (export get-user-dir get-site-lisp-dir get-lock-path get-package-dir
    get-read-json get-write-json get-depends-entry get-lock-entry
    get-resolve-declared-source get-declared-ref get-json-set get-json-sub
    get-package-root %package-version-dir
  ) ;export
  (begin

    (define (get-user-dir)
      (path-join (path-home) ".gf")
    ) ;define

    (define (get-site-lisp-dir)
      (path-join (get-user-dir) "site-lisp")
    ) ;define

    (define (get-lock-path)
      (path-join (get-user-dir) "gfproject.lock")
    ) ;define

    ;; 安装目录：site-lisp/<org>/<name>/<version>；version 缺省 HEAD。
    (define (get-package-dir package version)
      (unless (gfproject-package? package)
        (type-error "get-package-dir: expected a gfproject package" package)
      ) ;unless
      (path-join (get-site-lisp-dir)
        (gfproject-package-organization package)
        (gfproject-package-name package)
        (if (and (string? version) (> (string-length version) 0)) version "HEAD")
      ) ;path-join
    ) ;define

    ;; 读取 JSON 文件；不存在或非法时返回空对象。
    (define (get-read-json p)
      (if (path-file? p)
        (catch #t
          (lambda ()
            (let ((data (string->json (path-read-text p))))
              (if (json-object? data) data '(()))
            ) ;let
          ) ;lambda
          (lambda (type info) '(()))
        ) ;catch
        '(())
      ) ;if
    ) ;define

    ;; json-push 追加重复键、json-set 只替换已有键（缺失时原样返回），
    ;; 都不具备 upsert 语义；这里统一实现：已有键用 set 替换，
    ;; 缺失键（含空对象）用 push 新增。
    (define (get-json-set obj key value)
      (cond ((null? obj) (list (cons key value)))
            ((json-contains-key? obj key) (json-set obj key value))
            (else (json-push obj key value))
      ) ;cond
    ) ;define

    ;; 取子对象；缺失或为空时返回空对象。
    (define (get-json-sub obj key)
      (if (and (json-object? obj) (json-contains-key? obj key))
        (let ((sub (json-ref obj key)))
          (if (json-object? sub) sub '(()))
        ) ;let
        '(())
      ) ;if
    ) ;define

    (define (get-write-json p obj)
      (path-mkdir (path-parent p) #t #t)
      (path-write-text p (string-append (json->string obj 2) "\n"))
    ) ;define

    ;; depends 中的单个包声明；未注册返回 #f。
    (define (get-depends-entry config id)
      (and (json-object? config)
        (json-contains-key? config "depends")
        (let ((depends (json-ref config "depends")))
          (and (json-object? depends)
            (json-contains-key? depends id)
            (json-ref depends id)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; lock 中的单个包事实记录；未锁定返回 #f。
    (define (get-lock-entry lock id)
      (and (json-object? lock)
        (json-contains-key? lock "packages")
        (let ((packages (json-ref lock "packages")))
          (and (json-object? packages)
            (json-contains-key? packages id)
            (json-ref packages id)
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; 远程包的版本安装目录：site-lisp/<org>/<name>/<version>，
    ;; version 由 lock 记录（缺省 HEAD）。id 形如 name@organization；
    ;; 供 get-package-root 与 gf remove 共享，调用方自行保证 id 已登记。
    (define (%package-version-dir id)
      (let* ((at (string-index id #\@))
             (name (substring id 0 at))
             (org (substring id (+ at 1) (string-length id)))
             (lock-entry (get-lock-entry (get-read-json (get-lock-path)) id))
             (version (if lock-entry (json-ref-string lock-entry "version" "HEAD") "HEAD"))
            ) ;
        (path-join (get-site-lisp-dir) org name version)
      ) ;let*
    ) ;define

    ;; 已安装包的项目根：本地开发包取 depends.path（~ 展开）；
    ;; 远程包取 site-lisp/<org>/<name>/<version>，version 由 lock 记录（缺省 HEAD）。
    ;; id 形如 name@organization；未注册时返回 #f。
    (define (get-package-root id)
      (let* ((config (get-read-json (path-join (get-user-dir) "gfproject.json")))
             (entry (get-depends-entry config id))
            ) ;
        (cond ((not entry) #f)
              ((json-contains-key? entry "path")
               (let ((p (json-ref-string entry "path" "")))
                 (path-expanduser (path (if p p "")))
               ) ;let
              ) ;
              (else (%package-version-dir id))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 取 depends 中登记的 ref（仅远程 url 来源）；本地路径或未登记 ref 返回 #f。
    (define (get-declared-ref id)
      (let ((entry
              (get-depends-entry (get-read-json (path-join (get-user-dir) "gfproject.json"))
                id
              ) ;get-depends-entry
            ) ;entry
           ) ;
        (and entry
          (json-contains-key? entry "ref")
          (let ((ref (json-ref-string entry "ref" #f)))
            (and (string? ref) (> (string-length ref) 0) ref)
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; 解析短包名的来源：用户 depends 中登记的 url 或本地 path。
    ;; 返回 (cons kind source)；未注册时抛 value-error 并提示首次安装方式。
    (define (get-resolve-declared-source id)
      (let ((entry
              (get-depends-entry (get-read-json (path-join (get-user-dir) "gfproject.json"))
                id
              ) ;get-depends-entry
            ) ;entry
           ) ;
        (cond ((not entry)
               (value-error (string-append "gf get: unknown package \"" id
                              "\"; specify its repository for the first installation:\n"
                              "  gf get -g " id " --from <url-or-path>\n"
                              "or install directly:\n"
                              "  gf get -g <url-or-path>"
                            ) ;string-append
               ) ;value-error
              ) ;
              ((json-contains-key? entry "path")
               (cons 'path (json-ref-string entry "path" ""))
              ) ;
              ((json-contains-key? entry "url") (cons 'url (json-ref-string entry "url" "")))
              (else (value-error "gf get: depends entry has neither url nor path" id))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

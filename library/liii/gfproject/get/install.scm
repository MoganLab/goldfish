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

;; 用户级安装编排：解析来源 → 获取项目 → 校验包身份 → 落盘/登记 →
;; 自动编辑 ~/.gf/gfproject.json（depends + tools）与 gfproject.lock。
;; 本地路径是开发模式：不复制源码，depends 记 path，lock 记 version=dev。
;; 远程 URL 是安装模式：完整项目进入 site-lisp/<org>/<name>/HEAD/。

(define-library (liii gfproject get install)
  (import (scheme base)
    (liii error)
    (liii json)
    (liii os)
    (liii path)
    (liii shutil)
    (liii string)
    (liii gfproject config)
    (liii gfproject package)
    (liii gfproject get source)
    (liii gfproject get store)
    (liii gfproject get fetch)
  ) ;import
  (export get-install)
  (begin

    ;; 解析请求：返回 (list id-or-false kind source ref)。
    ;; arg 为短包名时查用户 depends 得到来源与登记 ref；--from 显式指定来源。
    ;; ref 仅来自显式 --ref 或已登记的 depends；本地路径模式 ref 恒为 #f。
    (define (%resolve-request arg from ref)
      (let ((kind (get-source-kind arg)))
        (cond
          (from
            (let ((from-kind (get-source-kind from)))
              (when (eq? from-kind 'package)
                (value-error "gf get: --from must be a url or path" from)
              ) ;when
              (list (if (eq? kind 'package) arg #f)
                from-kind
                (if (eq? from-kind 'path) (get-source-local-path from) from)
                (if (eq? from-kind 'url) ref #f)
              ) ;list
            ) ;let
          ) ;from
          ((eq? kind 'package)
           (let ((resolved (get-resolve-declared-source arg)))
             (list arg
               (car resolved)
               (if (eq? (car resolved) 'path)
                 (get-source-local-path (cdr resolved))
                 (cdr resolved)
               ) ;if
               ;; 短名重装复用登记 ref；显式 --ref 可覆盖
               (if (and (string? ref) (> (string-length ref) 0)) ref (get-declared-ref arg))
             ) ;list
           ) ;let
          ) ;
          (else (list #f
                  kind
                  (if (eq? kind 'path) (get-source-local-path arg) arg)
                  (if (eq? kind 'url) ref #f)
                ) ;list
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 获取项目目录与 lock 事实：本地直接使用工作副本；
    ;; 远程按 ref 克隆到临时目录（caller 负责清理），记录实际 commit。
    ;; ref 为 @latest 时先解析为最新稳定 tag 再克隆。
    ;; 返回 (list dir tmp-or-#f extra resolved-ref)：resolved-ref 为真实
    ;; ref（@latest 被解析后的 tag，其余原样），本地路径为 #f。
    (define (%acquire kind source ref)
      (if (eq? kind 'path)
        (begin
          (unless (path-dir? source)
            (value-error "gf get: local path is not a directory" source)
          ) ;unless
          (list (path source) #f '() #f)
        ) ;begin
        (let ((resolved-ref
                (if (and (string? ref) (string=? ref "@latest")) (get-resolve-latest source) ref)
              ) ;resolved-ref
             ) ;
          (let ((tmp (get-git-clone source resolved-ref)))
            (list tmp tmp (list (cons "commit" (get-git-commit tmp))) resolved-ref)
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 校验请求身份与仓库声明一致；--from/URL 首次安装时登记短名映射。
    (define (%check-identity requested-id package)
      (when
        (and requested-id (not (string=? requested-id (gfproject-package-id package))))
        (value-error (string-append "gf get: package identity mismatch: requested "
                       requested-id
                       ", repository declares "
                       (gfproject-package-id package)
                     ) ;string-append
        ) ;value-error
      ) ;when
    ) ;define

    ;; 同 id 已登记不同来源时拒绝，除非 force?。
    ;; ref 不同也视为来源变化（避免误覆盖分支槽位）。
    (define (%check-source-conflict config id kind source ref force?)
      (let ((entry (get-depends-entry config id)))
        (when (and entry (not force?))
          (let ((old (if (json-contains-key? entry "path")
                       (cons 'path (json-ref-string entry "path" ""))
                       (cons 'url (json-ref-string entry "url" ""))
                     ) ;if
                ) ;old
               ) ;
            (unless (and (eq? (car old) kind) (string=? (cdr old) source))
              (value-error (string-append "gf get: "
                             id
                             " is already registered with "
                             (cdr old)
                             "; use --force to replace the source"
                           ) ;string-append
              ) ;value-error
            ) ;unless
            ;; 同 url 但 ref 不同也要求 --force
            (when
              (and (eq? kind 'url) (not (string=? (or ref "") (or (get-declared-ref id) ""))))
              (value-error (string-append "gf get: "
                             id
                             " is already registered with ref \""
                             (get-declared-ref id)
                             "\"; use --force to change the ref"
                           ) ;string-append
              ) ;value-error
            ) ;when
          ) ;let
        ) ;when
      ) ;let
    ) ;define

    ;; 递归复制目录树 src -> dst（跨文件系统安全）。dst 必须不存在。
    (define (%copy-tree src dst)
      (path-mkdir dst #t #t)
      (vector-for-each
        (lambda (name)
          (let ((s (path-join src name)) (d (path-join dst name)))
            (if (path-dir? s) (%copy-tree s d) (path-copy s d))
          ) ;let
        ) ;lambda
        (path-list src)
      ) ;vector-for-each
    ) ;define

    ;; 把目录树移动到 target：优先 rename（同文件系统），失败回退递归 copy+remove。
    (define (%move-tree src target)
      (unless (path-rename src target)
        (%copy-tree src target)
        (rmtree (path->string src) #t)
      ) ;unless
    ) ;define

    ;; 远程安装：把临时目录移动到 site-lisp/<org>/<name>/<version>/（覆盖旧内容）。
    ;; version 为 ref 或 HEAD（无 ref 时）。
    (define (%install-tree tmp package ref)
      (let ((target
              (get-package-dir package
                (if (and (string? ref) (> (string-length ref) 0)) ref "HEAD")
              ) ;get-package-dir
            ) ;target
           ) ;
        (path-mkdir (path-parent target) #t #t)
        (when (path-dir? target)
          (rmtree (path->string target))
        ) ;when
        (%move-tree tmp target)
        target
      ) ;let
    ) ;define

    ;; depends 声明条目：来源与 local 标志；远程记 ref（可缺省），版本事实在 lock。
    (define (%depends-entry kind source ref)
      (if (eq? kind 'path)
        (json-push (json-push '(()) "path" source) "local" #t)
        (if (and (string? ref) (> (string-length ref) 0))
          (json-push (json-push '(()) "url" source) "ref" ref)
          (json-push '(()) "url" source)
        ) ;if
      ) ;if
    ) ;define

    ;; lock 事实条目：远程记 url/commit/ref/version 与安装相对路径；
    ;; 本地记 path 与 version=dev，不锁 commit。version 为 ref 或 HEAD/dev。
    (define (%lock-entry kind source ref root extra)
      (let ((base
              (if (eq? kind 'path)
                (json-push (json-push '(()) "path" source) "local" #t)
                (let ((b (json-push '(()) "url" source)))
                  (if (and (string? ref) (> (string-length ref) 0)) (json-push b "ref" ref) b)
                ) ;let
              ) ;if
            ) ;base
           ) ;
        (let loop
          ((rest extra) (acc base))
          (if (null? rest)
            (json-push acc
              "version"
              (cond ((eq? kind 'path) "dev")
                    ((and (string? ref) (> (string-length ref) 0)) ref)
                    (else "HEAD")
              ) ;cond
            ) ;json-push
            (loop (cdr rest) (json-push acc (caar rest) (cdar rest)))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 把包声明的 tools 写入用户配置：搬运 organization/module/description，
    ;; 加 package 绑定；同名工具已归属其他包时拒绝（force? 时覆盖）。
    ;; root 为已解析的项目根（本地工作副本或 site-lisp 安装目录）。
    (define (%register-tools config package root force?)
      (let* ((pkg-config (gfproject-read-file (path-join root "gfproject.json")))
             (pkg-tools (gfproject-extract-tools pkg-config))
             (id (gfproject-package-id package))
            ) ;
        (if (equal? pkg-tools '(()))
          config
          (let loop
            ((names (json-keys pkg-tools)) (tools (get-json-sub config "tools")))
            (if (null? names)
              (get-json-set config "tools" tools)
              (let* ((name (car names))
                     (tool (json-ref pkg-tools name))
                     (existing (and (json-object? tools) (json-contains-key? tools name) (json-ref tools name))
                     ) ;existing
                    ) ;
                (let ((owner (and existing
                               (json-contains-key? existing "package")
                               (json-ref-string existing "package" "")
                             ) ;and
                      ) ;owner
                     ) ;
                  (when (and owner (not (string=? owner id)) (not force?))
                    (value-error (string-append "gf get: tool \"" name
                                   "\" is already provided by " owner
                                   "; use --force to replace"
                                 ) ;string-append
                    ) ;value-error
                  ) ;when
                ) ;let
                (loop (cdr names)
                  (get-json-set tools
                    name
                    ;; tools_dir/tool 是包内自描述的相对定位，安装后由
                    ;; package 绑定推导项目根，不搬进用户配置。
                    (get-json-set (json-drop (json-drop tool "tools_dir") "tool") "package" id)
                  ) ;get-json-set
                ) ;loop
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 主入口：安装 arg（URL/路径/短名），返回 0；错误抛异常由 CLI 捕获。
    (define (get-install arg from ref force?)
      (let* ((request (%resolve-request arg from ref))
             (requested-id (car request))
             (kind (cadr request))
             (source (caddr request))
             (ref (cadddr request))
             (acquired (%acquire kind source ref))
             (dir (car acquired))
             (tmp (cadr acquired))
             (extra (caddr acquired))
             ;; @latest 已被解析为真实 tag；其余 ref 原样，本地为 #f。
             (resolved-ref (cadddr acquired))
            ) ;
        (catch #t
          (lambda ()
            (let* ((pkg+config (get-read-project-package dir))
                   (package (car pkg+config))
                   (id (gfproject-package-id package))
                   (config-path (path-join (get-user-dir) "gfproject.json"))
                   (config (get-read-json config-path))
                  ) ;
              (%check-identity requested-id package)
              (%check-source-conflict config id kind source ref force?)
              (let* ((root (if (eq? kind 'path) dir (%install-tree tmp package resolved-ref)))
                     (config (get-json-set config
                               "depends"
                               (get-json-set (get-json-sub config "depends")
                                 id
                                 (%depends-entry kind source ref)
                               ) ;get-json-set
                             ) ;get-json-set
                     ) ;config
                     (config (%register-tools config package root force?))
                     (lock (get-read-json (get-lock-path)))
                    ) ;
                (get-write-json config-path config)
                (get-write-json (get-lock-path)
                  (get-json-set lock
                    "packages"
                    (get-json-set (get-json-sub lock "packages")
                      id
                      (%lock-entry kind source resolved-ref root extra)
                    ) ;get-json-set
                  ) ;get-json-set
                ) ;get-write-json
                (display
                  (string-append "Installed "
                    id
                    " ("
                    (cond ((eq? kind 'path) "local development")
                          ((and (string? resolved-ref) (> (string-length resolved-ref) 0)) resolved-ref)
                          (else "HEAD")
                    ) ;cond
                    ") -> "
                    (path->string root)
                    "\n"
                  ) ;string-append
                ) ;display
                0
              ) ;let*
            ) ;let*
          ) ;lambda
          (lambda (type info)
            (when (and tmp (path-dir? tmp))
              (rmtree (path->string tmp) #t)
            ) ;when
            (apply error (cons type (if (list? info) info (list info))))
          ) ;lambda
        ) ;catch
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

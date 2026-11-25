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

;; gf remove 的卸载编排（gf get -g 的逆操作）：
;; 查 depends → 远程删 site-lisp 安装目录（本地开发包只清注册，不动工作副本）
;; → 清 depends/lock 条目 → 注销该包拥有的工具绑定。
;; 只注销 tools.<name>.package 等于该包 id 的绑定，他人归属的工具不动。

(define-library (liii gfproject remove)
  (import (scheme base)
    (scheme write)
    (liii error)
    (liii json)
    (liii list)
    (liii path)
    (liii shutil)
    (liii string)
    (liii gfproject get source)
    (liii gfproject get store)
  ) ;import
  (export gfproject-remove-package)
  (begin

    ;; 删键后可能得到空对象 '()，而 JSON 编码器把 '() 当数组；
    ;; 统一换回空对象 '(())，避免写出非法结构。
    (define (%json-object-or-empty obj)
      (if (null? obj) '(()) obj)
    ) ;define

    ;; 注销该包拥有的工具绑定：仅删 tools.<name>.package == id 的条目。
    (define (%unregister-tools config id)
      (let* ((tools (get-json-sub config "tools"))
             (owned
               (filter
                 (lambda (name)
                   (let ((tool (json-ref tools name)))
                     (and (json-object? tool)
                       (json-contains-key? tool "package")
                       (let ((owner (json-ref-string tool "package" "")))
                         (and (string? owner) (string=? owner id))
                       ) ;let
                     ) ;and
                   ) ;let
                 ) ;lambda
                 (json-keys tools)
               ) ;filter
             ) ;owned
            ) ;
        (if (null? owned)
          config
          (get-json-set config
            "tools"
            (%json-object-or-empty (apply json-drop (cons tools owned)))
          ) ;get-json-set
        ) ;if
      ) ;let*
    ) ;define

    ;; 删除远程安装目录；site-lisp 下已空的 <name>/<org> 目录一并清理。
    ;; site-lisp 根本身保留（可能还有其它包）。
    (define (%remove-install-dir root)
      (when (path-dir? root)
        (rmtree (path->string root))
      ) ;when
      (let loop
        ((dir (path-parent root)))
        (when
          (and (path-dir? dir)
            (path-starts-with? dir (get-site-lisp-dir))
            (not (path-equals? dir (get-site-lisp-dir)))
            (zero? (vector-length (path-list dir)))
          ) ;and
          (rmtree (path->string dir))
          (loop (path-parent dir))
        ) ;when
      ) ;let
    ) ;define

    ;; 主入口：卸载用户级包 id（name@organization），返回 0；错误抛异常由 CLI 捕获。
    ;; 远程包删安装目录；本地开发包只清注册，不触碰工作副本。
    (define (gfproject-remove-package id)
      (unless (and (string? id) (eq? (get-source-kind id) 'package))
        (value-error "gf remove: expected a package id (name@organization)" id)
      ) ;unless
      (let* ((config-path (path-join (get-user-dir) "gfproject.json"))
             (config (get-read-json config-path))
             (entry (get-depends-entry config id))
            ) ;
        (unless entry
          (let* ((depends (get-json-sub config "depends")) (installed (json-keys depends)))
            (value-error
              (string-append "gf remove: package \""
                id
                "\" is not installed"
                (if (null? installed)
                  ""
                  (string-append "; installed packages: " (string-join installed ", "))
                ) ;if
              ) ;string-append
            ) ;value-error
          ) ;let*
        ) ;unless
        (let* ((local? (json-contains-key? entry "path"))
               (root (if local? #f (%package-version-dir id)))
               (config
                 (%unregister-tools
                   (get-json-set config
                     "depends"
                     (%json-object-or-empty (json-drop (get-json-sub config "depends") id))
                   ) ;get-json-set
                   id
                 ) ;%unregister-tools
               ) ;config
               (lock-path (get-lock-path))
               (lock (get-read-json lock-path))
              ) ;
          (when root
            (%remove-install-dir root)
          ) ;when
          (get-write-json config-path config)
          (get-write-json lock-path
            (get-json-set lock
              "packages"
              (%json-object-or-empty (json-drop (get-json-sub lock "packages") id))
            ) ;get-json-set
          ) ;get-write-json
          (display
            (string-append "Removed "
              id
              (if local?
                " (local development; working copy kept)"
                (string-append " (" (path->string root) ")")
              ) ;if
              "\n"
            ) ;string-append
          ) ;display
          0
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii gfproject tools)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii json)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
    (liii gfproject config)
  ) ;import
  (export gfproject-find-tool-root
    gfproject-resolve-tool
    gfproject-resolve-tool-bundle
    gfproject-prepare-and-run-tool
    gfproject-run-tool
    gfproject-expand-tools-dir
    gfproject-parse-import-spec
    gfproject-tool-conflict?
    gfproject-error->string
    ;; re-export 自 (liii gfproject config)，保持旧 API 兼容
    gfproject-root
    gfproject-get-gf-lib
    gfproject-find-lib-path
    gfproject-find-local-path
    gfproject-extract-tools
    gfproject-deep-merge
    gfproject-load-config-bundle
    gfproject-load-config
  ) ;export
  (begin

    ;; 将 s7 的错误 info 转换为可读字符串：info 是 (格式串 参数...) 列表时
    ;; 完整格式化（替换 ~S/~A 等占位符），否则整体打印
    (define (gfproject-error->string info)
      (if (and (pair? info) (string? (car info)))
        (catch #t
          (lambda () (apply format #f info))
          (lambda (tag e) (format #f "~A" info))
        ) ;catch
        (format #f "~A" info)
      ) ;if
    ) ;define

    (define (gfproject-expand-tools-dir dir)
      (let ((p (path-expanduser dir)))
        (path->string
          (if (path-absolute? p)
            p
            (let ((root (gfproject-root)))
              (path-join (or root (getcwd)) p)
            ) ;let
          ) ;if
        ) ;path->string
      ) ;let
    ) ;define

    (define (gfproject-find-tool-root tool-name . rest)
      (let* ((tools-dir (if (pair? rest) (car rest) #f))
             (opt-gf-lib (if (pair? rest) (cdr rest) '()))
            ) ;
        (if (and tools-dir (not (string-null? tools-dir)))
          (let ((target (path-join (gfproject-expand-tools-dir tools-dir) tool-name)))
            (and (path-dir? target) (path->string target))
          ) ;let
          (let* ((gf-lib (gfproject--opt-gf-lib opt-gf-lib))
                 ;; ; [1131] 内置插件优先：cwd 下的同名目录仅作自定义工具的
                 ;; ; 兜底，不得遮蔽 gf-lib 侧的内置插件
                 (candidates (list (path-join gf-lib "tools" tool-name)
                               (path-join (path-parent gf-lib) "tools" tool-name)
                               (path-join (getcwd) "tools" tool-name)
                             ) ;list
                 ) ;candidates
                 (found (find path-dir? candidates))
                ) ;
            (and found (path->string found))
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (gfproject-resolve-tool command . opt-gf-lib)
      (let* ((bundle (apply gfproject-load-config-bundle opt-gf-lib))
             (merged-tools (bundle-ref bundle "merged_tools"))
            ) ;
        (and (json-contains-key? merged-tools command) (json-ref merged-tools command))
      ) ;let*
    ) ;define

    (define (gfproject-resolve-tool-bundle command . opt-gf-lib)
      (let* ((bundle (apply gfproject-load-config-bundle opt-gf-lib))
             (local-tools (bundle-ref bundle "local_tools"))
             (lib-tools (bundle-ref bundle "lib_tools"))
             (merged-tools (bundle-ref bundle "merged_tools"))
             (has-lib? (json-contains-key? lib-tools command))
            ) ;
        (and (json-contains-key? merged-tools command)
          (list (cons "has-local-override" (json-contains-key? local-tools command))
            (cons "has-lib-tool" has-lib?)
            (cons "lib_path" (bundle-ref bundle "lib_path"))
            (cons "local_path" (bundle-ref bundle "local_path"))
            (cons "lib_tools" lib-tools)
            (cons "local_tools" local-tools)
            (cons "merged-tool" (json-ref merged-tools command))
            (cons "lib-tool" (if has-lib? (json-ref lib-tools command) '(())))
          ) ;list
        ) ;and
      ) ;let*
    ) ;define

    (define (gfproject-parse-import-spec org module)
      (let ((mod-parts
              (map string->symbol
                (filter (lambda (s) (not (string-null? s))) (string-split module "/"))
              ) ;map
            ) ;mod-parts
           ) ;
        (cons (string->symbol org) mod-parts)
      ) ;let
    ) ;define

    ;; ; [1131] 内置工具隔离：本地 gfproject.json 注册的命令与内置注册表
    ;; ; 重名、且两者并非同一份配置文件时，判定为冲突。bin/gf 在仓库内
    ;; ; 开发时 lib_path 与 local_path 指向同一个文件，不算冲突。
    (define (gfproject-tool-conflict? command bundle)
      (let* ((lib-path (bundle-ref bundle "lib_path"))
             (local-path (bundle-ref bundle "local_path"))
             (same-source?
               (and lib-path
                 local-path
                 (string=? (path->string (path-resolve lib-path))
                   (path->string (path-resolve local-path))
                 ) ;string=?
               ) ;and
             ) ;same-source?
             (lib-tools (bundle-ref bundle "lib_tools"))
             (local-tools (bundle-ref bundle "local_tools"))
            ) ;
        (and (not same-source?)
          (json-contains-key? lib-tools command)
          (json-contains-key? local-tools command)
        ) ;and
      ) ;let*
    ) ;define

    ;; ; [1131] 冲突提示：内置插件不受本地同名注册影响，提示后继续用内置实现。
    ;; ; 直写 *stderr*（进程 fd）：工具的 main 可能直接 (exit)，若写入被捕获的
    ;; ; current-error-port，提示会随捕获 port 一起丢失
    (define (gfproject--warn-tool-conflict command bundle)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display (string-append "Warning: Tool '"
                   command
                   "' is a built-in tool (defined in "
                   (bundle-ref bundle "lib_path")
                   "); ignoring local registration in "
                   (bundle-ref bundle "local_path")
                   ".\n"
                 ) ;string-append
          p
        ) ;display
      ) ;let
    ) ;define

    ;; ; 在 rootlet 中执行 (import <import-spec>)，成功返回 #f，
    ;; ; 失败返回错误描述字符串
    (define (gfproject--try-import import-spec)
      (catch #t
        (lambda () (eval `(import ,import-spec) (rootlet)) #f)
        (lambda (tag info) (gfproject-error->string info))
      ) ;catch
    ) ;define

    ;; ; 调用已加载工具的 main 过程；返回整数退出码，
    ;; ; rootlet 中没有可调用的 main 时返回 #f
    (define (gfproject--invoke-main)
      (let ((main-proc
              (catch #t (lambda () (eval 'main (rootlet))) (lambda (tag info) #f))
            ) ;main-proc
           ) ;
        (if (procedure? main-proc)
          (let ((res
                  (catch #t
                    (lambda () (main-proc))
                    (lambda (tag info)
                      (display (string-append (gfproject-error->string info) "\n")
                        (current-error-port)
                      ) ;display
                      1
                    ) ;lambda
                  ) ;catch
                ) ;res
               ) ;
            (if (integer? res) res 0)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; ; 加载并运行工具模块（tool-root 已加入 *load-path* 之后调用）
    (define (gfproject--run-loaded-tool import-spec fail)
      (let ((import-err (gfproject--try-import import-spec)))
        (if import-err
          (fail "Error importing " (object->string import-spec) ":\n" import-err "\n")
          (or (gfproject--invoke-main)
            (fail "Error: Failed to find main function in "
              (object->string import-spec)
              ".\n"
            ) ;fail
          ) ;or
        ) ;if
      ) ;let
    ) ;define

    (define (gfproject-prepare-and-run-tool command tool-config gf-lib allow-fallback)
      (let* ((tool-name (if (json-object? tool-config)
                          (json-ref-string tool-config "tool" command)
                          command
                        ) ;if
             ) ;tool-name
             (tools-dir (if (json-object? tool-config) (json-ref-string tool-config "tools_dir" #f) #f)
             ) ;tools-dir
             (has-tools-dir? (and tools-dir (not (string-null? tools-dir))))
             (effective-fallback (if has-tools-dir? #f allow-fallback))
             (expanded-tools-dir (and has-tools-dir? (gfproject-expand-tools-dir tools-dir)))
            ) ;
        (define (fail . parts)
          (if effective-fallback
            #f
            (begin
              (for-each (lambda (p) (display p (current-error-port))) parts)
              1
            ) ;begin
          ) ;if
        ) ;define
        (if (not (json-object? tool-config))
          (fail "Error: Tool '" command "' config must be a JSON object.\n")
          (let ((org (json-ref-string tool-config "organization" #f))
                (module (json-ref-string tool-config "module" #f))
               ) ;
            (if (or (not org) (not module))
              (fail "Error: Tool '"
                command
                "' is not fully implemented (missing organization or module).\n"
              ) ;fail
              (let ((tool-root (gfproject-find-tool-root tool-name (or expanded-tools-dir tools-dir) gf-lib)
                    ) ;tool-root
                   ) ;
                (if (not tool-root)
                  (if has-tools-dir?
                    (fail "Error: Tool '"
                      command
                      "' directory not found: "
                      (path->string (path-join expanded-tools-dir tool-name))
                      "\n"
                    ) ;fail
                    (fail "Error: tools/" tool-name "/" org " directory not found.\n")
                  ) ;if
                  (begin
                    (set! *load-path* (cons tool-root *load-path*))
                    (gfproject--run-loaded-tool (gfproject-parse-import-spec org module) fail)
                  ) ;begin
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (gfproject-run-tool command . opt-gf-lib)
      (let ((bundle (apply gfproject-resolve-tool-bundle command opt-gf-lib)))
        (cond ((not bundle) #f)
              ;; ; [1131] 内置插件隔离：本地重名时提示并强制使用内置实现
              ((gfproject-tool-conflict? command bundle)
               (gfproject--warn-tool-conflict command bundle)
               (gfproject--dispatch-tool command bundle opt-gf-lib #t)
              ) ;
              (else (gfproject--dispatch-tool command bundle opt-gf-lib #f))
        ) ;cond
      ) ;let
    ) ;define

    ;; ; force-lib? 为 #t 时跳过本地配置，直接用内置（lib 侧）配置执行
    (define (gfproject--dispatch-tool command bundle opt-gf-lib force-lib?)
      (let* ((gf-lib (gfproject--opt-gf-lib opt-gf-lib))
             (has-local? (bundle-ref bundle "has-local-override"))
             (has-lib? (bundle-ref bundle "has-lib-tool"))
             (merged-tool (bundle-ref bundle "merged-tool"))
             (lib-tool (bundle-ref bundle "lib-tool"))
             (builtin-fallback? (member command '("help" "version" "eval" "load"
                                                  "repl" "run"))
             ) ;builtin-fallback?
            ) ;
        (cond (force-lib? (gfproject-prepare-and-run-tool command lib-tool gf-lib builtin-fallback?)
              ) ;force-lib?
              ((and has-local? has-lib?)
               (let ((ret (gfproject-prepare-and-run-tool command merged-tool gf-lib #t)))
                 (or ret
                   (gfproject-prepare-and-run-tool command lib-tool gf-lib builtin-fallback?)
                 ) ;or
               ) ;let
              ) ;
              (else (gfproject-prepare-and-run-tool command merged-tool gf-lib builtin-fallback?)
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

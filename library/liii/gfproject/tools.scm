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
    (liii error)
    (liii json)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
    (liii gfproject config)
    (liii gfproject external)
  ) ;import
  (export gfproject-find-tool-root
    gfproject-resolve-tool
    gfproject-resolve-tool-bundle
    gfproject-prepare-and-run-tool
    gfproject-run-tool
    gfproject-expand-tools-dir
    gfproject-parse-import-spec
    gfproject-find-project-root
    gfproject-tool-conflict?
    gfproject-error->string
    gfproject-tool-base-dir
    ;; re-export 自 (liii gfproject config)，保持旧 API 兼容
    gfproject-root
    gfproject-get-gf-lib
    gfproject-find-lib-path
    gfproject-find-local-path
    gfproject-find-user-path
    gfproject-extract-tools
    gfproject-deep-merge
    gfproject-load-config-bundle
    gfproject-load-config
    bundle-ref
  ) ;export
  (begin

    ;; 将 s7 的错误 info 转换为可读字符串，并追加缺失模块的 import 提示
    (define (gfproject-error->string info)
      (catch-error->string #f info)
    ) ;define

    ;; (gfproject-expand-tools-dir dir [base-dir])
    ;; base-dir 为 #f 时保持旧行为（相对项目根/cwd）；否则相对 base-dir 展开。
    ;; 用户层配置的 tools_dir 以 ~/.gf 为基准，项目层以项目根为基准。
    (define (gfproject-expand-tools-dir dir . rest)
      (let* ((base-dir (if (pair? rest) (car rest) #f)) (p (path-expanduser dir)))
        (path->string
          (if (path-absolute? p) p (path-join (or base-dir (gfproject-root) (getcwd)) p))
        ) ;path->string
      ) ;let*
    ) ;define

    ;; 工具配置所在的 gfproject.json 目录；#f 时回退旧基准。
    (define (%gfproject-config-dir path)
      (and path (path->string (path-parent path)))
    ) ;define

    ;; 命令最终采用的配置来源目录：项目层 > 用户层 > 内置层。
    (define (gfproject-tool-base-dir bundle)
      (cond ((bundle-ref bundle "has-local-override")
             (%gfproject-config-dir (bundle-ref bundle "local_path"))
            ) ;
            ((bundle-ref bundle "has-user-tool")
             (%gfproject-config-dir (bundle-ref bundle "user_path"))
            ) ;
            (else (%gfproject-config-dir (bundle-ref bundle "lib_path")))
      ) ;cond
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
                 ;; ; [1131] 内置插件优先：cwd 同名目录仅作兜底，不得遮蔽内置插件
                 ;; ; [1265] 候选统一 realpath 归一化，避免逻辑路径与物理路径形态不一致
                 (candidates (list (path-join gf-lib "tools" tool-name)
                               (path-join (path-parent gf-lib) "tools" tool-name)
                               (path-join (getcwd) "tools" tool-name)
                             ) ;list
                 ) ;candidates
                 (found (find path-dir? candidates))
                ) ;
            (and found (path->string (path-resolve found)))
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
             (lib-tools (bundle-ref bundle "lib_tools"))
             (user-tools (bundle-ref bundle "user_tools"))
             (local-tools (bundle-ref bundle "local_tools"))
             (merged-tools (bundle-ref bundle "merged_tools"))
             (has-lib? (json-contains-key? lib-tools command))
             (has-user? (json-contains-key? user-tools command))
             (has-local? (json-contains-key? local-tools command))
             (base-dir
               (cond (has-local? (%gfproject-config-dir (bundle-ref bundle "local_path")))
                     (has-user? (%gfproject-config-dir (bundle-ref bundle "user_path")))
                     (else (%gfproject-config-dir (bundle-ref bundle "lib_path")))
               ) ;cond
             ) ;base-dir
            ) ;
        (and (json-contains-key? merged-tools command)
          (list (cons "has-local-override" has-local?)
            (cons "has-user-tool" has-user?)
            (cons "has-lib-tool" has-lib?)
            (cons "lib_path" (bundle-ref bundle "lib_path"))
            (cons "user_path" (bundle-ref bundle "user_path"))
            (cons "local_path" (bundle-ref bundle "local_path"))
            (cons "lib_tools" lib-tools)
            (cons "user_tools" user-tools)
            (cons "local_tools" local-tools)
            (cons "merged-tool" (json-ref merged-tools command))
            (cons "lib-tool" (if has-lib? (json-ref lib-tools command) '(())))
            (cons "user-tool" (if has-user? (json-ref user-tools command) '(())))
            (cons "tool_base_dir" base-dir)
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

    ;; ; [1131] 内置工具隔离：用户/本地注册与内置注册表重名且来源不同时判定冲突；
    ;; ; 仓库内开发时 lib_path 与 local_path 指向同一文件，不算冲突。
    (define (%gfproject-same-source? a b)
      (and a
        b
        (string=? (path->string (path-resolve a)) (path->string (path-resolve b)))
      ) ;and
    ) ;define

    (define (gfproject-tool-conflict? command bundle)
      (let* ((lib-path (bundle-ref bundle "lib_path"))
             (user-path (bundle-ref bundle "user_path"))
             (local-path (bundle-ref bundle "local_path"))
             (lib-tools (bundle-ref bundle "lib_tools"))
             (user-tools (bundle-ref bundle "user_tools"))
             (local-tools (bundle-ref bundle "local_tools"))
             (has-lib? (json-contains-key? lib-tools command))
             (user-conflict? (and has-lib?
                               (json-contains-key? user-tools command)
                               (not (%gfproject-same-source? lib-path user-path))
                             ) ;and
             ) ;user-conflict?
             (local-conflict? (and has-lib?
                                (json-contains-key? local-tools command)
                                (not (%gfproject-same-source? lib-path local-path))
                              ) ;and
             ) ;local-conflict?
            ) ;
        (or user-conflict? local-conflict?)
      ) ;let*
    ) ;define

    ;; ; 冲突提示：内置插件不受用户/本地同名注册影响，提示后继续用内置实现。
    ;; ; 直写 *stderr*（进程 fd）：工具的 main 可能直接 (exit)，若写入被捕获的
    ;; ; current-error-port，提示会随捕获 port 一起丢失
    (define (gfproject--warn-tool-conflict command bundle)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port)))
            (lib-path (bundle-ref bundle "lib_path"))
            (user-tools (bundle-ref bundle "user_tools"))
            (local-tools (bundle-ref bundle "local_tools"))
           ) ;
        (let ((source (cond ((json-contains-key? user-tools command) (bundle-ref bundle "user_path"))
                            ((json-contains-key? local-tools command) (bundle-ref bundle "local_path"))
                            (else "<unknown>")
                      ) ;cond
              ) ;source
             ) ;
          (display (string-append "Warning: Tool '" command
                     "' is a built-in tool (defined in " lib-path
                     "); ignoring local registration in " source ".\n"
                   ) ;string-append
            p
          ) ;display
        ) ;let
      ) ;let
    ) ;define

    ;; ; 在 rootlet 中执行 (import <import-spec>)，成功返回 #f，
    ;; ; 失败返回错误描述字符串
    (define (gfproject--try-import import-spec)
      (catch #t
        (lambda () (eval `(import ,import-spec) (rootlet)) #f)
        (lambda (tag info) (catch-error->string tag info))
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
                      (display (string-append (catch-error->string tag info) "\n")
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
      (gfproject-prepare-and-run-tool* command tool-config gf-lib allow-fallback #f)
    ) ;define

    (define (gfproject-prepare-and-run-tool* command tool-config gf-lib
              allow-fallback base-dir
            ) ;gfproject-prepare-and-run-tool*
      (let* ((tool-name (if (json-object? tool-config)
                          (json-ref-string tool-config "tool" command)
                          command
                        ) ;if
             ) ;tool-name
             (tools-dir (if (json-object? tool-config) (json-ref-string tool-config "tools_dir" #f) #f)
             ) ;tools-dir
             (has-tools-dir? (and tools-dir (not (string-null? tools-dir))))
             (effective-fallback (if has-tools-dir? #f allow-fallback))
             (expanded-tools-dir (and has-tools-dir? (gfproject-expand-tools-dir tools-dir base-dir))
             ) ;expanded-tools-dir
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
                    ;; 外部项目：读取其自身 gfproject.json 的 domains，
                    ;; 把项目根与 domain 根注入 *load-path*，避免复制 domains。
                    (if has-tools-dir? (gfproject-external-project-load-path! tool-root) #f)
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
              ;; ; [1131] 内置插件隔离：用户/本地重名时提示并强制使用内置实现
              ((gfproject-tool-conflict? command bundle)
               (gfproject--warn-tool-conflict command bundle)
               (gfproject--dispatch-tool command bundle opt-gf-lib #t)
              ) ;
              (else (gfproject--dispatch-tool command bundle opt-gf-lib #f))
        ) ;cond
      ) ;let
    ) ;define

    ;; ; force-lib? 为 #t 时跳过用户/本地配置，直接用内置（lib 侧）配置执行
    (define (gfproject--dispatch-tool command bundle opt-gf-lib force-lib?)
      (let* ((gf-lib (gfproject--opt-gf-lib opt-gf-lib))
             (has-local? (bundle-ref bundle "has-local-override"))
             (has-user? (bundle-ref bundle "has-user-tool"))
             (has-lib? (bundle-ref bundle "has-lib-tool"))
             (merged-tool (bundle-ref bundle "merged-tool"))
             (lib-tool (bundle-ref bundle "lib-tool"))
             (base-dir (bundle-ref bundle "tool_base_dir"))
             (lib-base (%gfproject-config-dir (bundle-ref bundle "lib_path")))
             (builtin-fallback? (member command '("help" "version" "eval" "load"
                                                  "repl" "run"))
             ) ;builtin-fallback?
            ) ;
        (cond (force-lib? (gfproject-prepare-and-run-tool* command lib-tool
                            gf-lib builtin-fallback? lib-base
                          ) ;gfproject-prepare-and-run-tool*
              ) ;force-lib?
              ((and (or has-local? has-user?) has-lib?)
               (let ((ret (gfproject-prepare-and-run-tool* command merged-tool gf-lib #t base-dir)))
                 (or ret
                   (gfproject-prepare-and-run-tool* command lib-tool gf-lib
                     builtin-fallback? lib-base
                   ) ;gfproject-prepare-and-run-tool*
                 ) ;or
               ) ;let
              ) ;
              (else (gfproject-prepare-and-run-tool* command merged-tool gf-lib
                      builtin-fallback? base-dir
                    ) ;gfproject-prepare-and-run-tool*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

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
;; License for the specific language governing permissions and
;; limitations under the License.
;;

;; 外部项目加载：用户层 / 项目层通过 tools_dir 注册的工具有可能位于
;; 另一个独立项目（如 ~/git/gfcode）。该项目的 domains 只属于它自己的
;; gfproject.json，不能复制进用户配置，也不能与当前项目 domains 合并。
;; 这里在运行工具前读取外部项目自身配置，把项目根与 domain 根注入
;; *load-path*，使工具及其跨 domain 依赖（如扩展）能够被 import。
(define-library (liii gfproject external)
  (import (scheme base)
    (liii json)
    (liii path)
    (liii string)
    (liii gfproject config)
  ) ;import
  (export gfproject-find-project-root gfproject-external-project-load-path!)
  (begin

    ;; 从目录向上定位最近的 gfproject.json（外部项目根）。
    ;; 与 (liii goldtest-project) 的 %find-project-root 语义一致。
    (define (gfproject-find-project-root dir)
      (let loop
        ((current dir))
        (if (not current)
          #f
          (let* ((current-str (path->string current))
                 (config-path (path-join current "gfproject.json"))
                 (parent (path->string (path-parent current)))
                ) ;
            (cond ((path-file? config-path) current-str)
                  ((or (string-null? parent) (string=? parent current-str)) #f)
                  (else (loop (path parent)))
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%normalize-root root)
      (path->string (path-resolve (path root) #f))
    ) ;define

    (define (%absolute-root project-root root)
      (path->string (path-join project-root (if (string-null? root) "." root)))
    ) ;define

    ;; 项目 gfproject.json 中声明的全部 domain 根（去重、保持声明顺序）。
    ;; domain 根同样做 realpath 归一化：项目根已经归一化，若 domain 根只做
    ;; 词法拼接，macOS 上 /var -> /private/var 这类符号链接会让两者形态不一致。
    (define (%all-domain-roots project-root domains)
      (if (equal? domains '(()))
        '()
        (let loop
          ((names (json-keys domains)) (roots '()))
          (if (null? names)
            (reverse roots)
            (let* ((d (json-ref domains (car names)))
                   (root (json-ref-string d "root" ""))
                   (absolute (%absolute-root project-root root))
                  ) ;
              (loop (cdr names)
                (if (or (string-null? root) (member absolute roots string=?))
                  roots
                  (cons (%normalize-root absolute) roots)
                ) ;if
              ) ;loop
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 把外部项目的加载根注入 *load-path*：项目根 + 其 gfproject.json 中
    ;; 声明的全部 domain 根。
    ;;
    ;; 不按“工具所在 domain + depends-on”过滤，而是与 bin/gf 在项目内
    ;; 启动时的行为保持一致：项目内所有 domain 根都会参与库搜索。
    ;; 扩展（如 gfcode 的 review / simplify / web-search）可能跨 domain import。
    (define (gfproject-external-project-load-path! tool-root)
      (let ((project-root (gfproject-find-project-root tool-root)))
        (if (not project-root)
          #f
          (let* ((config-path (path-join project-root "gfproject.json"))
                 (config (gfproject-read-file config-path))
                 (domains (gfproject-extract-domains config))
                 (entries (cons (%normalize-root project-root) (%all-domain-roots project-root domains))
                 ) ;entries
                ) ;
            (for-each
              (lambda (entry)
                (if (not (member entry *load-path* string=?))
                  (set! *load-path* (cons entry *load-path*))
                ) ;if
              ) ;lambda
              (reverse entries)
            ) ;for-each
            #t
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

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

;; 远程包获取：浅克隆到临时目录，解析 HEAD commit，读取包元数据。
;; 未指定 ref 时直接取远端 HEAD；指定 ref 时按分支/标签/commit 检出。
;; commit 是写入 lock 的不可变事实。

(define-library (liii gfproject get fetch)
  (import (scheme base)
    (liii either)
    (liii error)
    (liii json)
    (liii list)
    (liii path)
    (liii semver)
    (liii shutil)
    (liii string)
    (liii subprocess)
    (liii uuid)
    (liii gfproject config)
    (liii gfproject package)
  ) ;import
  (export get-git-clone get-git-commit get-read-project-package
    get-resolve-latest
  ) ;export
  (begin

    ;; 执行 git 命令，失败时抛 os-error 并附 stderr。
    (define (%git args cwd)
      (let ((result (run-either (cons "git" args) :cwd cwd)))
        (if (either-right? result)
          (string-trim-both (to-right result))
          (let ((err (to-left result)))
            (error 'os-error
              (string-append "git "
                (string-join args " ")
                " failed: "
                (process-error-stderr err)
              ) ;string-append
            ) ;error
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 浅克隆 url 到临时目录；ref 为 #f 时取远端 HEAD，否则按 ref 检出。
    ;; ref 优先尝试 clone --branch <ref> --depth 1（分支/标签），失败时
    ;; 回退全量 clone 后 checkout <ref>（兼容 commit SHA 与禁用 shallow-by-ref 的服务端）。
    ;; 成功返回临时目录 path，调用方负责清理。
    (define (get-git-clone url ref)
      (unless (and (string? url) (> (string-length url) 0))
        (type-error "get-git-clone: expected a non-empty url" url)
      ) ;unless
      (let ((tmp (path-join (path-temp-dir) (string-append "gf-get-" (uuid4)))))
        (catch #t
          (lambda ()
            (if (and (string? ref) (> (string-length ref) 0))
              (begin
                ;; 先尝试按 ref 浅克隆；失败回退全量克隆 + checkout。
                (catch #t
                  (lambda ()
                    (%git (list "clone" "--depth" "1" "--branch" ref url (path->string tmp)) #f)
                  ) ;lambda
                  (lambda (type info)
                    (when (path-dir? tmp)
                      (rmtree (path->string tmp) #t)
                    ) ;when
                    ;; 全量克隆再 checkout 到目标 ref。
                    (%git (list "clone" "-q" url (path->string tmp)) #f)
                    (%git (list "checkout" ref) (path->string tmp))
                  ) ;lambda
                ) ;catch
              ) ;begin
              (%git (list "clone" "--depth" "1" url (path->string tmp)) #f)
            ) ;if
            tmp
          ) ;lambda
          (lambda (type info)
            (when (path-dir? tmp)
              (rmtree (path->string tmp) #t)
            ) ;when
            (apply error (cons type (if (list? info) info (list info))))
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

    ;; 解析目录中 git 仓库的 HEAD commit。
    (define (get-git-commit dir)
      (%git (list "rev-parse" "HEAD") (path->string dir))
    ) ;define

    ;; 从 git ls-remote --tags 的一行中提取 tag 名（去掉 refs/tags/ 前缀）。
    ;; 行形如 "<sha>\trefs/tags/<tag>"；annotated tag 的 peel 行
    ;; "<sha>\trefs/tags/<tag>^{}" 被排除（返回 #f）。
    ;; 解析失败或不是 tag 行时返回 #f。
    (define (%ls-remote-line->tag line)
      (let ((fields (string-split line #\tab)))
        (and (pair? fields)
          (pair? (cdr fields))
          (let ((ref (cadr fields)))
            (and (string-starts? ref "refs/tags/")
              (not (string-contains? ref "^{}"))
              (let ((tag (substring ref (string-length "refs/tags/") (string-length ref))))
                (and (> (string-length tag) 0) tag)
              ) ;let
            ) ;and
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; tag 是否为稳定 semver：semver-parse 合法且 prerelease 段为空。
    ;; prerelease 段为解析结果第三元素（caddr），空 list 表示正式版。
    (define (%stable-semver? tag)
      (let ((parsed (semver-parse tag)))
        (and parsed (null? (caddr parsed)))
      ) ;let
    ) ;define

    ;; 解析远端仓库的最新稳定 release tag（@latest 伪 ref 的实现）。
    ;; 通过 git ls-remote --tags 列出全部 tag，过滤出合法稳定 semver，
    ;; 按 SemVer 优先级取最高者，返回原始 tag 名（保留 v 前缀）。
    ;; 无任何稳定 tag 时抛 value-error（不回退 HEAD：latest release ≠ latest commit）。
    ;; 仅依赖 git 协议，对 GitHub/Gitee/自建/SSH 仓库通用。
    (define (get-resolve-latest url)
      (unless (and (string? url) (> (string-length url) 0))
        (type-error "get-resolve-latest: expected a non-empty url" url)
      ) ;unless
      (let* ((output (%git (list "ls-remote" "--tags" url) #f))
             (lines (if (string-null? output) '() (string-split output #\newline)))
             (tags (filter (lambda (t) t) (map %ls-remote-line->tag lines)))
             (stable (filter %stable-semver? tags))
            ) ;
        (if (null? stable)
          (value-error "gf get: no stable release tag found for @latest" url)
          (let loop
            ((best (car stable)) (rest (cdr stable)))
            (if (null? rest)
              best
              (loop (if (semver>? (car rest) best) (car rest) best) (cdr rest))
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; 读取项目 gfproject.json 并解析 package 元数据；缺失时报错。
    (define (get-read-project-package dir)
      (let* ((config (gfproject-read-file (path-join dir "gfproject.json")))
             (package (json->gfproject-package config))
            ) ;
        (unless package
          (value-error "gf get: repository has no package metadata in gfproject.json"
            (path->string dir)
          ) ;value-error
        ) ;unless
        (cons package config)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

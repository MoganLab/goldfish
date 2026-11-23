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
;; 未指定版本时直接取远端 HEAD；commit 是写入 lock 的不可变事实。

(define-library (liii gfproject get fetch)
  (import (scheme base)
    (liii either)
    (liii error)
    (liii json)
    (liii path)
    (liii shutil)
    (liii string)
    (liii subprocess)
    (liii uuid)
    (liii gfproject config)
    (liii gfproject package)
  ) ;import
  (export get-git-clone get-git-commit get-read-project-package)
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

    ;; 浅克隆 url 到临时目录；成功返回临时目录 path，调用方负责清理。
    (define (get-git-clone url)
      (unless (and (string? url) (> (string-length url) 0))
        (type-error "get-git-clone: expected a non-empty url" url)
      ) ;unless
      (let ((tmp (path-join (path-temp-dir) (string-append "gf-get-" (uuid4)))))
        (catch #t
          (lambda () (%git (list "clone" "--depth" "1" url (path->string tmp)) #f) tmp)
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

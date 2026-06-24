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
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; 格式化缓存：供 scheme-fmt / cpp-fmt 共享。
;; 缓存键为文件内容的 SHA-256；缓存目录按 Goldfish 版本隔离。

(define-library (liii goldfmt-cache)
  (import (liii base) (liii path) (liii hashlib))
  (export fmt-cache-base-dir fmt-cache-path fmt-cache-hit? fmt-cache-touch)
  (begin

    ;; 缓存根目录：~/.cache/goldfish/fmt/<version>/
    (define (fmt-cache-base-dir)
      (path->string (path-join (path-home) ".cache" "goldfish" "fmt" (version)))
    ) ;define

    ;; 缓存文件路径：<base-dir>/<前2位hash>/<剩余62位hash>
    (define (fmt-cache-path file-path)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (fmt-cache-base-dir))
            ) ;
        (path->string (path-join base prefix rest))
      ) ;let*
    ) ;define

    ;; 缓存命中：文件存在即认为已格式化。
    (define (fmt-cache-hit? file-path)
      (let ((cache (fmt-cache-path file-path)))
        (file-exists? cache)
      ) ;let
    ) ;define

    ;; 创建缓存条目（空文件）。按需创建两级父目录。
    (define (fmt-cache-touch file-path)
      (let* ((cache (fmt-cache-path file-path))
             (cache-dir (path->string (path-parent (path cache))))
            ) ;
        (unless (path-dir? (path cache-dir))
          (g_mkdir cache-dir)
        ) ;unless
        (path-touch (path cache))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

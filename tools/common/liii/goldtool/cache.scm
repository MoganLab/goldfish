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

;; Goldfish 工具共享缓存：空文件存在即表示对应内容已通过检查/处理。
;; 缓存键为文件内容的 SHA-256；缓存目录按工具名、Goldfish 版本与额外路径段隔离。

(define-library (liii goldtool cache)
  (import (scheme base) (liii base) (liii path) (liii hashlib) (liii os))
  (export goldtool-cache-base-dir goldtool-cache-path goldtool-cache-hit?
    goldtool-cache-touch
  ) ;export
  (begin

    ;; 缓存根目录：~/.cache/goldfish/<tool-name>/<version>/
    (define (goldtool-cache-base-dir tool-name)
      (path->string (path-join (path-home) ".cache" "goldfish" tool-name (version)))
    ) ;define

    ;; 缓存文件路径：
    ;;   <base-dir>/<extra-segments...>/<前2位hash>/<剩余62位hash>
    ;; hash 为文件内容的 SHA-256。
    (define (goldtool-cache-path tool-name file-path . extra-segments)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (goldtool-cache-base-dir tool-name))
            ) ;
        (path->string (apply path-join (append (list base) extra-segments (list prefix rest)))
        ) ;path->string
      ) ;let*
    ) ;define

    ;; 缓存命中：空文件存在即认为通过。
    (define (goldtool-cache-hit? file-path)
      (file-exists? file-path)
    ) ;define

    ;; 创建缓存条目（空文件）；按需创建父目录。
    (define (goldtool-cache-touch file-path)
      (let* ((cache file-path) (cache-dir (path->string (path-parent (path cache)))))
        (unless (path-dir? (path cache-dir))
          (mkdir cache-dir)
        ) ;unless
        (path-touch (path cache))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

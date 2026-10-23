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

;; gf check 规则级缓存：空文件存在即表示该规则检查通过。
;; 缓存键为文件内容的 SHA-256；缓存目录按 Goldfish 版本、规则名与限制值隔离。

(define-library (liii goldcheck cache)
  (import (scheme base) (liii base) (liii path) (liii hashlib))
  (export rule-cache-path rule-cache-hit? rule-cache-touch)
  (begin

    ;; 缓存根目录：~/.cache/goldfish/check/<version>/
    (define (check-cache-base-dir)
      (path->string (path-join (path-home) ".cache" "goldfish" "check" (version)))
    ) ;define

    ;; 缓存文件路径：
    ;;   <base-dir>/<rule-name>/<limit>/<前2位hash>/<剩余62位hash>
    ;; hash 为文件内容的 SHA-256。
    (define (rule-cache-path rule-name limit file-path)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (check-cache-base-dir))
            ) ;
        (path->string (path-join base rule-name (number->string limit) prefix rest))
      ) ;let*
    ) ;define

    ;; 缓存命中：空文件存在即认为该规则检查通过。
    (define (rule-cache-hit? rule-name limit file-path)
      (file-exists? (rule-cache-path rule-name limit file-path))
    ) ;define

    ;; 创建缓存条目（空文件）；按需创建两级父目录。
    (define (rule-cache-touch rule-name limit file-path)
      (let* ((cache (rule-cache-path rule-name limit file-path))
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

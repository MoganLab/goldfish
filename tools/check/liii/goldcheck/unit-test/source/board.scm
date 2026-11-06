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

(define-library (liii goldcheck unit-test source board)
  (import (scheme base)
    (liii path)
    (liii string)
    (liii list)
    (liii gfproject config)
    (liii goldcheck path)
    (liii goldcheck unit-test source parse)
  ) ;import
  (export find-base-for-source-path candidate-source-paths)
  (begin

    ;; 仓库分为四个板块：r7rs/（R7RS 标准库）、goldfish/（语言层）、
    ;; library/（标准库）、tools/（工具）。库实现只可能落在这三个板块目录
    ;; 之下，板块目录名同时是库名推导的路径锚点。
    (define board-roots '("goldfish" "r7rs" "library"))

    ;; 路径中的板块目录标记（带前后斜杠，避免匹配到同名普通目录）
    (define board-markers '("/goldfish/" "/r7rs/" "/library/"))

    (define (find-last-board-marker norm pos)
      ;; 找到从 pos 开始出现的最后一个板块目录标记，返回其起始下标；
      ;; 找不到时返回 #f。
      (let loop
        ((pos pos) (last-found -1))
        (let ((best -1))
          (for-each
            (lambda (marker)
              (let ((idx (string-index-of norm marker pos)))
                (if (and idx (> idx best)) (set! best idx))
              ) ;let
            ) ;lambda
            board-markers
          ) ;for-each
          (if (> best last-found)
            (loop (+ best 1) best)
            (if (= last-found -1) #f last-found)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (find-base-for-source-path source-path)
      ;; 源码所在板块目录（仓库根或 <板块> 根）：取路径中最后一个
      ;; /goldfish/ 、/r7rs/ 或 /library/ 之前的部分。
      (let* ((actual (resolve-repo-file source-path)) (norm (normalize-sep actual)))
        (let loop
          ((pos 0) (last-found #f))
          (let ((idx (find-last-board-marker norm pos)))
            (if idx
              (loop (+ idx 1) idx)
              (if last-found
                (substring norm 0 last-found)
                (let ((repo (gfproject-root)))
                  (if repo repo "")
                ) ;let
              ) ;if
            ) ;if
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

    (define (candidate-source-paths base-dir spec)
      ;; 库标识符 -> 候选源码路径列表：依次尝试各板块根目录下的同一相对路径。
      ;; 板块根目录既可能是源码所在工程根（base-dir）下的板块目录，
      ;; 也可能是仓库根下的板块目录。
      (let ((rel-scm (lib-spec->relative-source-path spec)))
        (map resolve-repo-file
          (append
            (map (lambda (root) (path->string (path-join base-dir root rel-scm)))
              board-roots
            ) ;map
            (map (lambda (root) (string-append root "/" rel-scm)) board-roots)
          ) ;append
        ) ;map
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

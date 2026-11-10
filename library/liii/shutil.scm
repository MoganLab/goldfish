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

(define-library (liii shutil)
  (export which rmtree)
  (import (scheme base) (liii base))
  (begin

    ;; which 在 PATH 或指定目录中查找可执行文件，
    ;; 找到返回路径字符串，找不到返回 #f。
    (define* (which cmd (path #f)) (if path (g_which cmd path) (g_which cmd)))

    ;; rmtree 递归删除目录树（目录及其全部内容）。
    ;; 删除成功返回 #t；ignore-errors 为 #t 时失败返回 #f，
    ;; 否则抛出 file-not-found-error 或 os-error。
    (define* (rmtree path (ignore-errors #f))
      (if ignore-errors (g_rmtree path #t) (g_rmtree path))
    ) ;define*

  ) ;begin
) ;define-library

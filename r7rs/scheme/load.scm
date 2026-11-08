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

;; (scheme load) - R7RS-small 6.12 eval
;;
;; R7RS-small 对 (load filename) 的规定：
;;   (load filename)                             (procedure)
;;   (load filename environment)                 (procedure)
;; filename 必须是字符串。load 读取文件并按顺序求值其中的每个 datum。
;; 可选参数 environment 指定求值环境，省略时使用实现定义的环境
;; （Goldfish 取交互环境 interaction-environment 语义，即顶层环境）。
;; 返回值 unspecified。文件不存在是 file error（R7RS 6.12）。
;;
;; 实现直接复用 s7 原生 load：其语义与 R7RS 一致——按序求值文件内容、
;; 可选第二参数为求值环境、文件不可读时报错误。

(define-library (scheme load)
  (import (scheme base))
  (export load)
  (begin
  ) ;begin
) ;define-library

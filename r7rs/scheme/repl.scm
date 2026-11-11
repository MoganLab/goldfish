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

;; (scheme repl) - R7RS-small 6.12 Eval: interaction-environment
;;
;; R7RS-small 对 (interaction-environment) 的规定：
;;   (interaction-environment)                   (procedure)
;; 返回交互式求值环境说明符（environment specifier），包含实现定义的一组绑定
;; （通常为 REPL 默认可用的超集），可传递给 eval 或 load 在其中求值表达式。
;;
;; 在 Goldfish Scheme 中，交互环境（顶层环境）即为底层 s7 的 (rootlet)。

(define-library (scheme repl)
  (import (scheme base))
  (export interaction-environment)
  (begin

    (define (interaction-environment)
      (rootlet)
    ) ;define

  ) ;begin
) ;define-library

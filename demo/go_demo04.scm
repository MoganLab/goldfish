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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (scheme base) (liii go))

;; 与 go_demo04.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示 select 多路复用配合 ((timeout ms) => proc) 语法实现超时控制模式（Timeout Pattern）。

(define (main)
  ;; ch: 无缓冲通道，且无发送者，接收操作必然阻塞
  (define ch (make-chan))

  ;; select 配合 ((timeout ms) => proc) 实现超时控制：
  ;; 底层基于 C++ wait-set 事件驱动等待；
  ;; 若 ch 在 100 毫秒内未就绪，则自动触发超时分支，
  ;; 并将实际等待耗时（毫秒数）直接传递给单参过程处理。
  (select ((chan-recv! ch) => (lambda (v) (display "got: ") (display v) (newline)))
   ((timeout 100)
    =>
    (lambda (ms)
      (display (string-append "timeout after: " (number->string ms) "ms\n"))
    ) ;lambda
   ) ;
  ) ;select
) ;define

(main)

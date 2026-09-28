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

;; 与 go_demo03.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示 select 多路复用机制与非阻塞 else 分支（采用 Scheme 惯用的 => 与 else 语法）。

(define (main)
  ;; ready-ch: 容量为 1 的缓冲通道，预先写入一个值，使其处于可立即读取的就绪状态
  (define ready-ch (make-chan 1))
  (chan-send! ready-ch "ready-value")

  ;; not-ready-ch: 无缓冲通道且无发送者，处于未就绪状态（读取会阻塞）
  (define not-ready-ch (make-chan))

  ;; 演示 1：多个分支中有一个分支就绪
  ;; 使用 ((chan-recv! ch) => proc) 语法，就绪时接收值自动传递给单参过程；
  ;; select 选择就绪的 ready-ch 分支，忽略未就绪分支和 else 兜底分支。
  (select ((chan-recv! ready-ch)
           =>
           (lambda (v) (display "got ready: ") (display v) (newline))
          ) ;
   ((chan-recv! not-ready-ch)
    =>
    (lambda (v) (display "got notReady: ") (display v) (newline))
   ) ;
   (else (display "default: no case ready\n"))
  ) ;select

  ;; 演示 2：所有通道分支均未就绪
  ;; 当所有通道均阻塞且存在 else 分支时，select 会以非阻塞方式立即执行 else 兜底分支
  (select ((chan-recv! not-ready-ch)
           =>
           (lambda (v) (display "unexpected: ") (display v) (newline))
          ) ;
    (else (display "default: notReady not ready\n"))
  ) ;select
) ;define

(main)

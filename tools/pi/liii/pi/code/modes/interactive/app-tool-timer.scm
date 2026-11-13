;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 运行中 bash 工具卡片 Elapsed 计时器推进：
;; bash 命令执行期间（编译、测试、长时间任务等），每隔 1000ms（1秒）触发一次整帧渲染，
;; 驱动卡片内部 Elapsed 时间自动推进并上屏，对齐 pi 运行中卡片的时间变化行为。
(define-library (liii pi code modes interactive app-tool-timer)
  (export %tool-timer-frame-interval-ms
    pi-app-has-pending-bash-tool?
    pi-app-tool-timer-tick!
  ) ;export
  (import (scheme base)
    (liii date-time)
    (liii list)
    (liii tui)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
  ) ;import
  (begin

    (define %tool-timer-frame-interval-ms 1000)

    ;; 检查当前是否有运行中的 bash 工具调用：
    ;; 优先扫描 tool-cards 注册表，再兜底扫描 chat 容器最新子卡片
    (define (pi-app-has-pending-bash-tool? app)
      (or
        (let loop
          ((cards (%pi-app-tool-cards app)))
          (cond ((null? cards) #f)
                ((tool-card-pending-bash? (cdar cards)) #t)
                (else (loop (cdr cards)))
          ) ;cond
        ) ;let
        (let loop
          ((children (reverse (container-children (%pi-app-chat app)))))
          (cond ((null? children) #f)
                ((tool-card-pending-bash? (car children)) #t)
                (else (loop (cdr children)))
          ) ;cond
        ) ;let
      ) ;or
    ) ;define

    ;; 运行中 bash 工具卡片 Elapsed 计时器推进：
    ;; 无运行中 bash 时复位计时戳并返回 #f；
    ;; 有运行中 bash 时，首拍记录当前时间戳，之后每隔 1000ms 触发整帧重绘，
    ;; 驱动 bash 卡片中的 Elapsed 计时文本推进，返回 #t 表示本拍执行了重绘。
    (define (pi-app-tool-timer-tick! app)
      (if (not (pi-app-has-pending-bash-tool? app))
        (begin
          (when (pi-app-tool-timer-last-tick app)
            (%pi-app-tool-timer-last-tick-set! app #f)
          ) ;when
          #f
        ) ;begin
        (let ((now (current-monotonic-millis)) (last (pi-app-tool-timer-last-tick app)))
          (cond ((not last) (%pi-app-tool-timer-last-tick-set! app now) #f)
                ((>= (- now last) %tool-timer-frame-interval-ms)
                 (%pi-app-tool-timer-last-tick-set! app now)
                 (pi-app-render! app)
                 #t
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

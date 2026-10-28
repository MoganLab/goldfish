;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;; implied. See the License for the specific language governing
;; permissions and limitations under the License.
;;

;; snake 的鼠标交互：把点击坐标翻译为转向指令。
;; 坐标系约定：棋盘格 (gx,gy) 的屏幕位置为 (origin-x+1+gx, origin-y+1+gy)，
;; 其中 origin 是棋盘 Box（含双线边框）左上角的屏幕坐标。蛇头同理偏移。

(define-library (demo tui snake mouse)
  (export snake-click->dir snake-mouse-press->dir)
  (import (scheme base) (liii base) (liii tui mouse) (demo tui snake logic))
  (begin

    ;; 点击屏幕坐标 -> 方向。与蛇头重合返回 #f。
    ;; 方向取 |dx| 与 |dy| 中较大的主轴；相等时取水平方向。
    (define (snake-click->dir game click-x click-y origin-x origin-y)
      (let* ((head (snake-head game))
             (hx (+ origin-x 1 (car head)))
             (hy (+ origin-y 1 (cadr head)))
             (dx (- click-x hx))
             (dy (- click-y hy))
            ) ;
        (cond ((and (= dx 0) (= dy 0)) #f)
              ((>= (abs dx) (abs dy)) (if (> dx 0) 'right 'left))
              (else (if (> dy 0) 'down 'up))
        ) ;cond
      ) ;let*
    ) ;define

    ;; mouse-event 分流：只处理左键 press，其余返回 #f
    (define (snake-mouse-press->dir game event origin-x origin-y)
      (if (and (mouse-event? event)
            (eq? (mouse-event-type event) 'press)
            (eq? (mouse-event-button event) 'left)
          ) ;and
        (snake-click->dir game
          (mouse-event-x event)
          (mouse-event-y event)
          origin-x
          origin-y
        ) ;snake-click->dir
        #f
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

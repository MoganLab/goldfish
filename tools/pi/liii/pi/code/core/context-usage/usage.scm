;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core context-usage usage)
  (import (scheme base) (liii base))
  (export pi-context-usage? make-pi-context-usage pi-context-usage-percent
    pi-context-usage-window pi-context-usage-known?
  ) ;export
  (begin

    ;; 上下文占用 record：百分比 + 窗口容量 + 是否已知
    ;; known? 为 #f 表示压缩后尚无新响应，占比未知（对齐上游显示 "?"）
    (define*-record-type <pi-context-usage>
      (make-pi-context-usage (percent #f) (window 0) (known? #f))
      pi-context-usage?
      (percent pi-context-usage-percent)
      (window pi-context-usage-window)
      (known? pi-context-usage-known?)
    ) ;define*-record-type

  ) ;begin
) ;define-library

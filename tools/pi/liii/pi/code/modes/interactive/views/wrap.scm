;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 卡片正文折行：统一 user / thinking / assistant 三类消息的折行口径。
;; 背景：alt-screen 整帧渲染时按终端宽度裁剪每行（truncate-to-width），
;; 卡片正文若不折行，超出宽度的部分会被直接吞掉；折行后按显示宽度逐段输出，
;; 中文等宽字符按字符宽度逐字折行。
(define-library (liii pi code modes interactive views wrap)
  (export %card-content-width %card-body-lines)
  (import (scheme base) (liii list) (liii tui text-width))
  (begin

    ;; 卡片正文可用宽度：终端宽度减去左右留白，至少保留 1 列
    (define (%card-content-width width left-pad right-pad)
      (max 1 (- width left-pad right-pad))
    ) ;define

    ;; 折行并逐段生成最终行：
    ;; - text 为卡片正文（可含 \n，按段落分别折行，空行保留）；
    ;; - left-pad / right-pad 为正文左右留白列数（参与内容宽度计算）；
    ;; - style-fn 接收折行后的每一段，返回该段对应的最终行列表
    ;;   （可加前缀、背景色或 ANSI 样式）。
    (define (%card-body-lines text width left-pad right-pad style-fn)
      (append-map style-fn
        (wrap-text text (%card-content-width width left-pad right-pad))
      ) ;append-map
    ) ;define

  ) ;begin
) ;define-library

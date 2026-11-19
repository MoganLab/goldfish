;; border-render-top
;; 渲染容器组件顶边框行，支持可选内嵌标题。
;;
;; 语法
;; ----
;; (border-render-top border width [title])
;;
;; 参数
;; ----
;; border : border-style?
;; 边框风格实例。
;;
;; width : integer?
;; 边框总宽度。
;;
;; title : string? (可选)
;; 嵌入顶边框的标题文本。
;;
;; 返回值
;; ----
;; string?
;; 渲染出的顶边框单行文本。

(import (scheme base) (liii check) (liii tui text-width) (liii tui box border))

(check-set-mode! 'report-failed)

(let ((single (border-style-resolve 'single))
      (ascii (border-style-resolve 'ascii))
      (none (border-style-resolve 'none))
     ) ;
  ;; ASCII 顶行
  (check (border-render-top ascii 10) => "+--------+")
  ;; Unicode single 顶行
  (let ((top10 (border-render-top single 10)))
    (check (display-width top10) => 10)
    (check top10 => "┌────────┐")
  ) ;let
  ;; 'none 风格顶行返回空字符串
  (check (border-render-top none 10) => "")

  ;; 嵌入标题
  (let ((top-title (border-render-top single 16 "Menu")))
    (check (display-width top-title) => 16)
    (check top-title => "┌─ Menu ───────┐")
  ) ;let

  ;; 嵌入 CJK 标题
  (let ((top-cjk (border-render-top single 16 "菜单")))
    (check (display-width top-cjk) => 16)
    (check top-cjk => "┌─ 菜单 ───────┐")
  ) ;let
) ;let

(check-report)

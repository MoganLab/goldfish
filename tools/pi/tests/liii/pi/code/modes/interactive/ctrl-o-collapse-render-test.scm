(import (scheme base)
  (liii check)
  (liii string)
  (liii list)
  (liii tui)
  (liii pi code modes interactive bash)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 全屏（备用屏）模式下的 bash 输出展开/折叠：
;; 帧高度固定为终端高度，展开与折叠都只影响对话记录视口内的可见内容，
;; 不再出现 inline 模式下"内容收缩导致尾部残留"的问题。
(let* ((buf (open-output-string))
       (term (make-standard-terminal :output-port buf
               :detect-size-fn (lambda () '(80 24))))
       (bash-exec (lambda (cmd)
                    (values
                      (string-join
                        (map (lambda (i) (string-append "line-" (number->string i)))
                          (iota 40))
                        "\n")
                      0)))
       (app (make-pi-app :terminal term :handler (lambda (a m) #t) :bash-handler bash-exec))
       (ed (pi-app-editor app))
       (screen (pi-app-screen app))
      ) ;
  (pi-app-render! app)
  (component-handle-input ed "!seq 1 40")
  (component-handle-input ed 'enter)
  (pi-app-render! app)
  ;; 第一次 Ctrl+O：展开（40 行全部渲染）
  (pi-app-toggle-tool-output! app)
  (pi-app-render! app)
  (let* ((expanded (alt-screen-lines screen))
         (expanded-text (string-join (map strip-ansi expanded) "\n"))
        ) ;
    ;; 全屏帧高度固定为终端高度
    (check (length expanded) => 24)
    ;; 展开态能看到输出首行（视口跟随末尾，40 行输出会滚出顶部，
    ;; 这里断言末尾若干行仍在视口内）
    (check-true (string-contains? expanded-text "line-39"))
    ;; 第二次 Ctrl+O：折叠收缩
    (pi-app-toggle-tool-output! app)
    (pi-app-render! app)
    (let* ((collapsed (alt-screen-lines screen))
           (collapsed-text (string-join (map strip-ansi collapsed) "\n"))
           (out (get-output-string buf))
          ) ;
      ;; 帧高度不变（固定视口，不存在 inline 的收缩残留）
      (check (length collapsed) => 24)
      ;; 折叠态给出预览与省略提示
      (check-true (string-contains? collapsed-text "ctrl+o to expand"))
      (check (string-contains? out "Tool output: collapsed") => #t)
    ) ;let*
  ) ;let
) ;let*

(check-report)

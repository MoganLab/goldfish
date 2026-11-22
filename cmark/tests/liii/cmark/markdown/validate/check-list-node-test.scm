;; check-list-node
;; 校验列表、列表项、引用块与 Alert 节点的形状与枚举值。
;;
;; 语法
;; ----
;; (check-list-node node)
;;
;; 参数
;; ----
;; node : any
;; 待校验的容器节点。
;;
;; 返回值
;; ----
;; unspecified
;; 校验通过时无返回值。
;;
;; 错误处理
;; ----
;; 枚举值非法、字段缺失或子块非法时抛出 md-stem-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown validate))

(check-set-mode! 'report-failed)

(check-true (procedure? check-list-node))

;; 合法容器节点
(check-true (check-list-node '(bullet-list tight (item (p "a")))))
(check-true (check-list-node '(bullet-list loose)))
(check-true
  (check-list-node '(ordered-list 10 one-paren tight (item (p "a"))))
) ;check-true
(check-true (check-list-node '(item (p "a"))))
(check-true (check-list-node '(item checked (p "a"))))
(check-true (check-list-node '(item)))
(check-true (check-list-node '(blockquote (p "引用"))))
(check-true (check-list-node '(alert note (p "提示"))))

;; tightness 枚举错误
(check-catch 'md-stem-error (check-list-node '(bullet-list fat (item))))
;; 有序列表字段缺失或枚举错误
(check-catch 'md-stem-error (check-list-node '(ordered-list 1 dot tight (item))))
(check-catch 'md-stem-error (check-list-node '(ordered-list -1 period tight (item))))
(check-catch 'md-stem-error (check-list-node '(ordered-list "1" period tight (item))))
(check-catch 'md-stem-error (check-list-node '(ordered-list 1 period)))
;; alert 枚举错误与字段缺失
(check-catch 'md-stem-error (check-list-node '(alert note? (p "x"))))
(check-catch 'md-stem-error (check-list-node '(alert)))
;; 容器子块非法
(check-catch 'md-stem-error (check-list-node '(blockquote (no-such-block))))
(check-catch 'md-stem-error (check-list-node '(bullet-list tight (item (bad-node)))))
(check-catch 'md-stem-error (check-list-node '(alert note 42)))

(check-report)

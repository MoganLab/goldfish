(import (liii check) (scheme base) (liii json))

(check-set-mode! 'report-failed)

;; R7RS 4.2.8 节标准示例验证
(check `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) => #(10 5 2 4 3 8))

;; 空向量与无 unquote 常量向量
(check #() => #())
(check #(1 2 3) => #(1 2 3))
(check #("hello" "world") => #("hello" "world"))

;; 单个与多个 unquote
(let ((x 42))
  (check `#(,x) => #(42))
) ;let
(let ((a 1) (b 2))
  (check `#(,a ,b 3) => #(1 2 3))
) ;let

;; unquote-splicing 展开
(let ((lst '(2 3 4)))
  (check `#(1 ,@lst 5) => #(1 2 3 4 5))
) ;let
(check `#(,@'()) => #())
(let ((x '(a b)))
  (check `#(,@x) => #(a b))
) ;let

;; 显式宏调用 (quasiquote #( ... ))
(check (quasiquote #(10 5 (unquote (sqrt 4)))) => #(10 5 2))
(check (quasiquote #()) => #())
(check (quasiquote #(1 2 3)) => #(1 2 3))

;; 向量嵌套向量
(let ((x 99))
  (check `#(#(1 ,x)) => #(#(1 99)))
) ;let

;; 列表与向量相互嵌套
(let ((x "val"))
  (check `(#(1 ,x) 2) => '(#(1 "val") 2))
  (check `#(1 (,x 2) 3) => '#(1 ("val" 2) 3))
) ;let

;; JSON 数组模板构造场景验证
(let ((prompt "hello"))
  (check `(("messages" . #((("role" . "user") ("content" . ,prompt)))))
    =>
    '(("messages" . #((("role" . "user") ("content" . "hello")))))
  ) ;check
  (check
    (json->string `(("messages" . #((("role" . "user") ("content" . ,prompt))))))
    =>
    "{\"messages\":[{\"role\":\"user\",\"content\":\"hello\"}]}"
  ) ;check
) ;let

(check-report)

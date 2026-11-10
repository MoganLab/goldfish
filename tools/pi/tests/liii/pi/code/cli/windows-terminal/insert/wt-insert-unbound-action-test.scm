;; wt-insert-unbound-action
;; 在 JSONC 文本中局部加入 Alt+Enter 的 unbound 动作并保留其他内容。
;;
;; 语法
;; ----
;; (wt-insert-unbound-action text) -> string
;;
;; 参数
;; ----
;; text : string
;; Windows Terminal settings.json 的 JSONC 文本。
;;
;; 返回值
;; ----
;; string
;; 返回局部插入后的文本；已解绑或存在用户自定义键位时保持原文不变。
;;

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii pi code cli windows-terminal)
) ;import

(check-set-mode! 'report-failed)

(define %fixtures-dir
  (cond ((path-exists? (path "tests/fixtures/windows-terminal"))
         "tests/fixtures/windows-terminal"
        ) ;
        ((path-exists? (path "tools/pi/tests/fixtures/windows-terminal"))
         "tools/pi/tests/fixtures/windows-terminal"
        ) ;
        (else "../../tools/pi/tests/fixtures/windows-terminal")
  ) ;cond
) ;define

(define (read-fixture name)
  (path-read-text (path-join %fixtures-dir name))
) ;define

;; 1. 对 default-binding.jsonc 进行局部插入
(let* ((orig (read-fixture "default-binding.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  ;; 原有注释与配置保持原样
  (check-true (string-contains? fixed "Windows Terminal settings fixture"))
  (check-true (string-contains? fixed "defaultProfile"))
  (check-true (string-contains? fixed "ctrl+shift+f"))
  ;; 插入了 unbound 配置
  (check-true (string-contains? fixed "unbound"))
  (check-true (string-contains? fixed "alt+enter"))
  ;; 修改后语义变为已解绑
  (check (wt-detect-alt-enter-status fixed) => 'already-unbound)
  ;; 幂等性：再次调用结果不变
  (check (wt-insert-unbound-action fixed) => fixed)
) ;let*

;; 2. 对已是 unbound 的配置不进行任何修改（幂等且不追加重复项）
(let* ((orig (read-fixture "already-unbound.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check fixed => orig)
) ;let*

;; 3. 对用户自定义快捷键不覆盖
(let* ((orig (read-fixture "custom-binding.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check fixed => orig)
) ;let*

;; 4. actions 数组为空的情况
(let* ((orig (read-fixture "actions-empty.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check-true (string-contains? fixed "unbound"))
  (check (wt-detect-alt-enter-status fixed) => 'already-unbound)
) ;let*

;; 5. 缺省 actions 的情况（在根对象追加 actions）
(let* ((orig (read-fixture "no-actions.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check-true (string-contains? fixed "\"actions\""))
  (check-true (string-contains? fixed "unbound"))
  (check (wt-detect-alt-enter-status fixed) => 'already-unbound)
) ;let*

;; 6. keybindings 旧格式
(let* ((orig (read-fixture "keybindings-old.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check-true (string-contains? fixed "unbound"))
  (check (wt-detect-alt-enter-status fixed) => 'already-unbound)
) ;let*

;; 7. 复杂 JSONC（保留行注释、块注释、尾逗号、字符串内括号）
(let* ((orig (read-fixture "jsonc-comments-trailing-comma.jsonc"))
       (fixed (wt-insert-unbound-action orig))
      ) ;
  (check-true (string-contains? fixed "/* Global settings block */"))
  (check-true (string-contains? fixed "// Existing action"))
  (check-true (string-contains? fixed "Test {brackets [in] string // not comments}")
  ) ;check-true
  (check-true (string-contains? fixed "unbound"))
  (check (wt-detect-alt-enter-status fixed) => 'already-unbound)
) ;let*

(check-report)

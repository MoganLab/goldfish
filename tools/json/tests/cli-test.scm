(set! *load-path* (cons "tools/json" *load-path*))

(import (scheme base) (scheme write) (liii check) (liii base) (liii goldjson))

(check-set-mode! 'report-failed)

;; 1. parse-json-args 基础与默认选项
(let ((p (parse-json-args '())))
  (check (cdr (assq 'indent p)) => 2)
  (check (cdr (assq 'compact p)) => #f)
  (check (cdr (assq 'sort-keys p)) => #f)
  (check (cdr (assq 'ensure-ascii p)) => #f)
  (check (cdr (assq 'json-lines p)) => #f)
  (check (cdr (assq 'validate p)) => #f)
  (check (cdr (assq 'help p)) => #f)
  (check (cdr (assq 'positionals p)) => '())
) ;let

;; 2. parse-json-args 显式选项
(let ((p (parse-json-args '("-i" "4" "-s" "--ensure-ascii" "in.json" "out.json"))))
  (check (cdr (assq 'indent p)) => 4)
  (check (cdr (assq 'sort-keys p)) => #t)
  (check (cdr (assq 'ensure-ascii p)) => #t)
  (check (cdr (assq 'positionals p)) => '("in.json" "out.json"))
) ;let

(let ((p (parse-json-args '("--indent=3" "--tab"))))
  ;; 后面的 --tab 覆盖 indent 为 "\t"
  (check (cdr (assq 'indent p)) => "\t")
) ;let

(let ((p (parse-json-args '("-c"))))
  (check (cdr (assq 'compact p)) => #t)
  (check (cdr (assq 'indent p)) => #f)
) ;let

(let ((p (parse-json-args '("--json-lines" "-v"))))
  (check (cdr (assq 'json-lines p)) => #t)
  (check (cdr (assq 'validate p)) => #t)
) ;let

(let ((p (parse-json-args '("-h"))))
  (check (cdr (assq 'help p)) => #t)
) ;let

;; 3. 非法参数校验
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (parse-json-args '("-i")) => #f)
  (check (parse-json-args '("-i" "-1")) => #f)
  (check (parse-json-args '("-i" "abc")) => #f)
  (check (parse-json-args '("--indent=-5")) => #f)
  (check (parse-json-args '("--unrecognized-flag")) => #f)
  (set-current-error-port orig-err)
) ;let

;; 4. run-goldjson 逻辑测试
;; 4.1 --help 返回 0
(let ((out (open-output-string)))
  (let ((orig (current-output-port)))
    (set-current-output-port out)
    (check (run-goldjson '("--help")) => 0)
    (set-current-output-port orig)
  ) ;let
) ;let

;; 4.2 位置参数过多返回 1
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (run-goldjson '("a.json" "b.json" "c.json")) => 1)
  (set-current-error-port orig-err)
) ;let

;; 4.3 输入文件不存在返回 1
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (run-goldjson '("non-existent-file-1244.json")) => 1)
  (set-current-error-port orig-err)
) ;let

;; 4.4 格式化与美化（重定向 stdin / stdout）
(let* ((in (open-input-string "{\"z\":1,\"a\":2}"))
       (out (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (let ((ret (run-goldjson '("--sort-keys"))))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (check ret => 0)
    (check (get-output-string out) => "{\n  \"a\": 2,\n  \"z\": 1\n}\n")
  ) ;let
) ;let*

;; 4.5 紧凑单行化
(let* ((in (open-input-string "{\n  \"a\": 1,\n  \"b\": 2\n}"))
       (out (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (let ((ret (run-goldjson '("--compact"))))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (check ret => 0)
    (check (get-output-string out) => "{\"a\":1,\"b\":2}\n")
  ) ;let
) ;let*

;; 4.6 语法错误拦截（返回 1）
(let* ((in (open-input-string "{\"bad\": 123,}"))
       (out (open-output-string))
       (err (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
       (orig-err (current-error-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (set-current-error-port err)
  (let ((ret (run-goldjson '())))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (set-current-error-port orig-err)
    (check ret => 1)
  ) ;let
) ;let*

;; 4.7 验证模式 --validate
(let* ((in (open-input-string "{\"valid\": true}"))
       (out (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (let ((ret (run-goldjson '("--validate"))))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (check ret => 0)
    ;; 验证模式不产生输出内容
    (check (get-output-string out) => "")
  ) ;let
) ;let*

;; 4.8 JSON Lines 逐行流式处理
(let* ((in (open-input-string "{\"b\":1,\"a\":2}\n{\"d\":3,\"c\":4}\n"))
       (out (open-output-string))
       (orig-in (current-input-port))
       (orig-out (current-output-port))
      ) ;
  (set-current-input-port in)
  (set-current-output-port out)
  (let ((ret (run-goldjson '("--json-lines" "--compact" "--sort-keys"))))
    (set-current-input-port orig-in)
    (set-current-output-port orig-out)
    (check ret => 0)
    (check (get-output-string out) => "{\"a\":2,\"b\":1}\n{\"c\":4,\"d\":3}\n")
  ) ;let
) ;let*

(check-report)

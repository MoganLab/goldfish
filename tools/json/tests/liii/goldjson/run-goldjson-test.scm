;; run-goldjson
;; 执行 gf json 的主流程：解析参数并完成 JSON 校验、格式化或转换。
;;
;; 语法
;; ----
;; (run-goldjson args)
;;
;; 参数
;; ----
;; args : list?
;; 命令行参数字符串列表，如 '("--sort-keys" "in.json")。
;; 输入默认读自当前输入端口，输出写到当前输出端口，
;; 错误提示写到当前错误端口。
;;
;; 返回值
;; -----
;; integer
;; 进程退出码：0 表示成功，1 表示参数或解析错误。
;;
;; 说明
;; ----
;; 支持格式化、紧凑化、排序、--validate 只校验、--json-lines 逐行处理，
;; 以及 INFILE/OUTFILE 位置参数重定向。

(set! *load-path* (cons "tools/json" *load-path*))

(import (scheme base) (scheme write) (liii check) (liii goldjson))

(check-set-mode! 'report-failed)

;; 1. --help 返回 0（输出重定向到字符串端口）
(let ((out (open-output-string)))
  (let ((orig (current-output-port)))
    (set-current-output-port out)
    (check (run-goldjson '("--help")) => 0)
    (set-current-output-port orig)
  ) ;let
) ;let

;; 2. 位置参数过多返回 1
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (run-goldjson '("a.json" "b.json" "c.json")) => 1)
  (set-current-error-port orig-err)
) ;let

;; 3. 输入文件不存在返回 1
(let ((err (open-output-string)) (orig-err (current-error-port)))
  (set-current-error-port err)
  (check (run-goldjson '("non-existent-file-1244.json")) => 1)
  (set-current-error-port orig-err)
) ;let

;; 4. 格式化与键排序（重定向 stdin / stdout）
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

;; 5. 紧凑单行化
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

;; 6. 语法错误拦截（返回 1）
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

;; 7. 验证模式 --validate：只校验不产生输出
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
    (check (get-output-string out) => "")
  ) ;let
) ;let*

;; 8. JSON Lines 逐行流式处理
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

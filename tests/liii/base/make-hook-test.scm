(import (liii check)
        (liii base)
        (liii os)
        (liii string)
        (scheme file))

;; make-hook - 创建 hook 对象
;;
;; make-hook 是 S7 内置函数，不在 R7RS 标准中定义。
;; 返回一个 hook（过程对象），可用于注册和触发回调函数。
;;
;; ---------------------------------------------------------------------------
;; 用法
;; ---------------------------------------------------------------------------
;;
;; (make-hook 'arg1 'arg2 ...)
;;
;; 创建一个新的 hook，接受指定的参数名。参数通过 (hook 'arg) 在回调中获取。
;;
;; ---------------------------------------------------------------------------
;; 示例
;; ---------------------------------------------------------------------------
;;
;; (define h (make-hook 'code))
;; (set! (hook-functions h) (list (lambda (hook) (hook 'code))))
;; (h 42)  ; 触发回调，code 为 42

;; make-hook 返回一个过程
(check (procedure? (make-hook)) => #t)
(check (procedure? (make-hook 'x)) => #t)
(check (procedure? (make-hook 'a 'b)) => #t)

;; hook-functions 默认值是空列表
(check (hook-functions (make-hook)) => '())

;; 设置 hook-functions 并触发回调
(let ((h (make-hook 'code))
      (result #f))
  (set! (hook-functions h)
        (list (lambda (hook)
                (set! result (hook 'code)))))
  (h 42)
  (check result => 42))

;; 多个回调按顺序执行
(let ((h (make-hook 'x))
      (results '()))
  (set! (hook-functions h)
        (list (lambda (hook) (set! results (cons (hook 'x) results)))
              (lambda (hook) (set! results (cons (* 2 (hook 'x)) results)))))
  (h 5)
  ;; 执行顺序：先第二个回调，再第一个回调（因为 cons 构建列表）
  ;; 实际上 for-each 是按列表顺序执行的，所以先第一个再第二个
  (check results => '(10 5)))

;; 无参数的 hook
(let ((h (make-hook))
      (called #f))
  (set! (hook-functions h)
        (list (lambda (hook) (set! called #t))))
  (h)
  (check called => #t))

;; 多参数 hook
(let ((h (make-hook 'a 'b))
      (result '()))
  (set! (hook-functions h)
        (list (lambda (hook)
                (set! result (list (hook 'a) (hook 'b))))))
  (h 1 2)
  (check result => '(1 2)))

;; *exit-hook* 在 exit 时被触发
;; 由于 exit 会结束进程，通过子进程方式测试
(let ((flag-file "tests/liii/base/exit-hook-flag.txt"))
  ;; 清理可能存在的旧文件
  (when (file-exists? flag-file)
    (delete-file flag-file))
  ;; 在子进程中运行：设置 *exit-hook* 并调用 exit
  (os-call "bin\\gf.exe -e \"(set! (hook-functions *exit-hook*) (list (lambda (hook) (with-output-to-file \\\"tests/liii/base/exit-hook-flag.txt\\\" (lambda () (display \\\"ok\\\") (newline)))))) (exit 0)\"")
  ;; 验证 exit-hook 被触发（文件被创建）
  (check (file-exists? flag-file) => #t)
  ;; 验证文件内容
  (when (file-exists? flag-file)
    (let ((content (call-with-input-file flag-file
                     (lambda (port)
                       (let loop ((chars '()))
                         (let ((c (read-char port)))
                           (if (eof-object? c)
                             (list->string (reverse chars))
                             (loop (cons c chars)))))))))
      (check (string-contains? content "ok") => #t))
    ;; 清理测试文件
    (delete-file flag-file)))

(check-report)

(import (scheme base)
        (scheme process-context)
        (liii check))

(check-set-mode! 'report-failed)

;; [1104] 回归测试：命令行参数必须是合法 UTF-8 字节序列
;;
;; 背景：Windows 上 CRT 的 argv 使用 ANSI 代码页（中文系统为 GBK），
;; 中文参数作为原始字节进入 Scheme 层后不是合法 UTF-8，
;; 拼入 HTTP 请求体会触发服务端 JSON 解析错误
;; （Invalid UTF-8 middle byte 0xc3，即 GBK 的“用”= D3 C3）。
;;
;; 批量运行（gf test --all）时命令行均为 ASCII，本测试自然通过；
;; 手工验证方法（Windows 中文系统，修复前失败、修复后通过）：
;;   bin/gf tests/scheme/process-context/command-line-test.scm 用一句话回答
(for-each
  (lambda (arg)
    (check (bytevector? (string->utf8 arg)) => #t))
  (command-line))

(check-report)

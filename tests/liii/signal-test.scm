;; (liii signal) 模块函数分类索引
;;
;; signal 提供跨平台的系统信号处理机制，支持信号捕获回调、忽略、恢复默认动作与主动触发。
;; 底层采用安全点（Safe-point）派发架构，保证异步信号安全（Async-Signal-Safe）。

;; ==== 常见用法示例 ====
(import (liii check) (liii signal))

(check-set-mode! 'report-failed)

;; 示例1：注册自定义信号回调并主动触发
(let ((handled #f))
  (signal-trap 'sigusr1 (lambda (sig) (set! handled sig)))
  (signal-raise 'sigusr1)
  (check handled => 'sigusr1)
  (signal-trap 'sigusr1 'default)
) ;let

;; 示例2：忽略指定信号并在稍后恢复默认行为
(let ((old (signal-trap 'sigpipe 'ignore)))
  (check (signal-get 'sigpipe) => 'ignore)
  (check (signal-trap 'sigpipe 'default) => 'ignore)
  (check (signal-get 'sigpipe) => 'default)
) ;let

;; 示例3：错误防御校验
(check-catch 'type-error (signal-trap "sigint" 'default))
(check-catch 'type-error (signal-trap 'sigint "invalid-handler"))
(check-catch 'value-error (signal-trap 'invalid-sig 'default))
(check-catch 'type-error (signal-get 123))
(check-catch 'value-error (signal-get 'invalid-sig))
(check-catch 'type-error (signal-raise "sigint"))
(check-catch 'value-error (signal-raise 'invalid-sig))

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/signal "signal-trap"
;;   bin/gf doc liii/signal "signal-get"
;;   bin/gf doc liii/signal "signal-raise"

;; ==== 函数分类索引 ====

;; 一、信号注册与动作设置
;;   signal-trap      - 为指定系统信号注册处理动作（回调、'default、'ignore）

;; 二、信号状态查询
;;   signal-get       - 获取指定系统信号当前绑定的处理动作

;; 三、信号派发与触发
;;   signal-raise     - 向当前进程主动发送指定系统信号

(check-report)

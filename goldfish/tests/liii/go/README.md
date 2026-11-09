# (liii go) 模块函数分类索引

`(liii go)` 参考 Go 语言的 CSP 并发模型，为 Goldfish Scheme 提供多核并发能力：
每个后台 worker 线程独占一个 s7 解释器会话，线程间通过 channel 以深拷贝方式
传递数据，天然免疫数据竞争，无 GIL 瓶颈。

使用时导入：

```scheme
(import (liii go))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
bin/gf doc liii/go "函数名"
```

或者在所有库中查找：

```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、Channel（通道，跨线程通信）

创建与谓词：

- `make-chan` - 创建通道，可指定缓冲容量（0 为无缓冲 rendezvous）
- `chan?` - 判断是否为通道

收发：

- `chan-send!` - 发送值（可选超时毫秒数）
- `chan-recv!` - 接收值（可选超时与默认值；关闭且读空返回 eof-object）
- `chan-try-send!` / `chan-try-recv!` - 非阻塞收发

生命周期：

- `chan-close!` - 关闭通道（幂等，唤醒所有等待者）
- `chan-closed?` - 查询关闭状态

多路复用：

- `select` - 等待多个通道操作中第一个就绪者，支持 timeout 与 else 分支

### 二、并发任务

- `go` - 将代码调度到后台 worker 线程池并发执行
- `go-call` - 过程版本的 go，将函数与参数调度到后台 worker 执行
- `go-apply` - 将 `(apply f args)` 投递到后台 worker，结果按 go-result 协议回传
- `go-apply/source` - go-apply 的源码级变体，自由变量在调用方给定的捕获环境中解析
- `go-result` - 同 go，但返回结果 channel（`(ok value)` \| `(error tag args)`），异常不死等
- `go-result-recv!` - 接收并拆包结果 channel，任务出错时重抛原异常
- `go-worker-count` - 查询 worker 线程池配置上限（等于硬件核心数）
- `go-active-worker-count` - 查询当前已创建的 worker 线程数

### 三、Context（协作式任务取消）

- `make-context` / `make-timeout-context` - 创建可取消/超时自动取消的上下文
- `context?` / `context-done?` - 谓词与状态查询
- `context-cancel!` - 主动取消，唤醒所有等待者
- `context-channel` - 取出完成通知 channel，可直接用于 select

### 四、Fiber（单会话内轻量协程，M:N 阶段一）

- `spawn-fiber` - 创建协程并入就绪队列
- `fiber-yield!` - 主动让出执行权
- `fiber-scheduler-run!` - 启动调度器（全阻塞时报 deadlock 错误）
- `make-fiber-chan` / `fiber-chan?` - fiber 专用通道（无界缓冲）
- `fiber-send!` / `fiber-recv!` - 协程间收发（recv 挂起不占物理线程）

### 五、调度器内部接口

- `%set-scheduler-idle-return!` - 关闭调度器空闲 park 策略，空闲时直接返回调用方
- `%run-worker-task` - worker 会话的任务执行入口
- `%drain-suspended!` - 消费 worker 会话中挂起的调度事件
- `%go-spawn` - 将任务直接投递到后台 worker 线程池的底层入口

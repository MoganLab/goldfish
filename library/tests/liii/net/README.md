# (liii net) 模块函数分类索引

`(liii net)` 提供极简的 TCP 网络传输层能力，接口对齐 Go 标准库 `net` 包的 `Listener`/`Conn` 抽象，并遵循 Guile 的函数式风格。

## 查看函数文档

```bash
gf doc liii/net
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/net "net-listen"
```

## 函数分类索引

### 一、监听器
- `net-listen` - 创建并绑定 TCP 监听器
- `net-listener?` - 判断对象是否为 net-listener
- `net-listener-port` - 读取监听器实际绑定的本地端口
- `net-listener-addr` - 读取监听器地址字符串
- `net-listener-close` - 关闭监听器并释放底层套接字

### 二、连接
- `net-dial` - 主动建立到远端地址的 TCP 连接
- `net-accept` - 在监听器上接收一个入站连接
- `net-conn?` - 判断对象是否为 net-conn
- `net-conn-peer-addr` - 读取对端地址字符串
- `net-conn-read` - 函数式读取数据（返回 bytevector 或 EOF）
- `net-conn-write` - 写入字符串或 bytevector 数据
- `net-conn-close` - 关闭连接

### 三、资源作用域组合子
- `call-with-net-listener` - Guile 风格监听器作用域（dynamic-wind 保证关闭）
- `call-with-net-conn` - Guile 风格连接作用域（dynamic-wind 保证关闭）

### 四、应用层
- `http-serve-once` - 单次 HTTP 回调服务（本地随机端口，可选自定义 handler）

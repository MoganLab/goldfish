# (liii fs-watch) 模块函数分类索引

`(liii fs-watch)` 提供文件系统事件监听能力（API 对齐 Python watchdog），支持事件处理器、observer 生命周期与后台 worker 泵。

## 查看函数文档

```bash
gf doc liii/fs-watch
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/fs-watch "make-fs-observer"
```

## 函数分类索引

### 一、事件记录
- `make-fs-event` - 构造文件系统事件记录
- `fs-event?` - 判断对象是否为文件系统事件记录
- `fs-event-type` - 读取事件类型
- `fs-event-path` - 读取事件目标路径
- `fs-event-dest-path` - 读取事件目标路径（仅 moved 事件有值）

### 二、事件处理器
- `make-fs-event-handler` - 创建文件系统事件处理器
- `fs-event-handler?` - 判断对象是否为事件处理器记录
- `fs-handler-on-modified` - 读取 modified 回调槽
- `fs-handler-on-created` - 读取 created 回调槽
- `fs-handler-on-deleted` - 读取 deleted 回调槽
- `fs-handler-on-moved` - 读取 moved 回调槽

### 三、Observer 生命周期
- `make-fs-observer` - 创建文件系统监听 observer
- `fs-observer?` - 判断对象是否为 observer
- `fs-observer-schedule!` - 注册监听项
- `fs-observer-start!` - 启动监听并派发后台泵线程
- `fs-observer-stop!` - 停止监听（幂等）

### 四、分发与监听泵
- `fs-watch-dispatch!` - 按事件类型调用对应回调
- `fs-watch-make-dispatcher` - 由源码列表重建回调表
- `fs-watch-handler->src-list` - 将回调槽序列化为源码字符串列表
- `fs-watch-pump` - worker 会话侧的监听泵
- `fs-watch-start%` - 带前置校验的底层 watcher 启动包装
- `%fs-event-from-alist` - 把 C 层事件 alist 转换为事件记录

## 平台差异

- Windows/Linux：提供具体文件事件（created / modified / deleted / moved）
- macOS：kqueue 后端只报目录级 `modified`（path 为监听目录本身）

## 集成测试

`fs-watch-integration-test.scm` 覆盖真实 watcher + go worker 的全链路验证。

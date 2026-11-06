# (liii psutil) 模块函数分类索引

`(liii psutil)` 提供进程内存、线程数与系统启动时刻的查询能力，适合进程监控、资源统计与系统探针场景。

## 查看函数文档

```bash
gf doc liii/psutil
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/psutil "process-memory-info"
```

## 函数分类索引

### 一、进程内存信息
- `process-memory-info` - 查询进程的内存信息，返回 memory-info 记录
- `memory-info?` - 判断对象是否为 memory-info 记录
- `memory-info-resident-set-memory-size` - 读取驻留物理内存字节数
- `memory-info-virtual-memory-size` - 读取虚拟内存字节数
- `memory-info-proportional-set-memory-size` - 读取按比例分摊的物理内存字节数

### 二、进程线程信息
- `process-num-threads` - 查询进程的线程数

### 三、系统信息
- `boot-time` - 获取系统启动时刻（Unix Epoch 秒数）

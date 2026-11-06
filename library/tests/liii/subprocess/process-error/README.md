# (liii subprocess process-error) 模块函数分类索引

`(liii subprocess process-error)` 定义子进程执行失败时使用的记录类型，保存退出码、标准错误、标准输出和原始命令。

## 查看函数文档

```bash
gf doc liii/subprocess/process-error
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/subprocess/process-error "process-error-message"
```

## 函数分类索引

### 一、构造与识别
- `make-process-error` - 构造子进程错误记录
- `process-error?` - 判断对象是否为子进程错误记录

### 二、记录访问
- `process-error-code` - 获取子进程退出码
- `process-error-stderr` - 获取标准错误输出
- `process-error-stdout` - 获取标准输出
- `process-error-command` - 获取原始命令

### 三、错误信息
- `process-error-message` - 按 stderr、stdout、退出码的优先级生成可读信息

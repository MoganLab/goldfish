# (liii gfproject tools) 模块函数分类索引

`(liii gfproject tools)` 提供 Goldfish Scheme 插件与扩展工具的动态查找、配置解析、路径展开与加载运行机制。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
gf doc "函数名"
```
或者指定库名：
```bash
gf doc liii/gfproject/tools "函数名"
```

## 函数分类索引

### 一、工具查找与解析
- `gfproject-find-tool-root` - 查找工具的根目录路径
- `gfproject-resolve-tool` - 解析工具的配置信息
- `gfproject-resolve-tool-bundle` - 解析包含本地与内置源的完整工具配置包
- `gfproject-expand-tools-dir` - 展开用户波浪号或相对路径的工具目录
- `gfproject-parse-import-spec` - 将组织与模块字符串转换为 Scheme 库导入规范
- `gfproject-tool-conflict?` - 检测工具配置中是否存在本地与内置冲突

### 二、工具执行与错误格式化
- `gfproject-prepare-and-run-tool` - 准备执行环境并运行指定工具
- `gfproject-run-tool` - 顶层调度执行已注册的工具
- `gfproject-error->string` - 将工具加载或运行中的异常转换为可读字符串

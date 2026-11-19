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
- `gfproject-resolve-tool-bundle` - 解析包含内置/用户/项目来源的完整工具配置包
- `gfproject-expand-tools-dir` - 展开波浪号或相对路径的工具目录，可指定基准目录
- `gfproject-tool-base-dir` - 返回工具配置所属的 gfproject.json 目录
- `gfproject-parse-import-spec` - 将组织与模块字符串转换为 Scheme 库导入规范
- `gfproject-tool-conflict?` - 检测工具配置中是否存在内置与用户/项目注册冲突

### 二、工具执行与错误格式化
- `gfproject-prepare-and-run-tool` - 准备执行环境并运行指定工具
- `gfproject-run-tool` - 顶层调度执行已注册的工具
- `gfproject-error->string` - 将工具加载或运行中的异常转换为可读字符串

## 工具解析优先级

```text
builtin > project/user merge > user only
```

- 命令存在于内置层时始终使用内置实现；用户层或项目层同名注册会打印 Warning 并忽略。
- 非内置命令：项目层字段覆盖用户层字段，未出现的字段保留用户层值。
- `tools_dir` 为相对路径时：用户层以 `~/.gf` 为基准，项目层以项目根为基准，内置层以
  内置 `gfproject.json` 所在目录为基准。

## 外部项目加载

对于带 `tools_dir` 的工具，运行前会从工具根向上查找最近的 `gfproject.json`：

- 找到时，把它所在的项目根以及该项目声明的全部 domain 根加入 `*load-path*`（去重、幂等）。
- 找不到时，仅保留工具根，保持旧的 `tools_dir` 行为。
- 只影响当前工具进程，不在启动时为所有命令预加载外部项目，也不改变 `gf test` 的加载路径。

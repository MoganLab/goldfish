# (liii pi code modes interactive common) 模块函数索引

`(liii pi code modes interactive common)` 提供交互模式公共辅助函数与 Git 分支监控。

使用时导入：

```scheme
(import (liii pi code modes interactive common))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc <函数名>
```

## 函数分类索引

### 平台与键位
- `%alt-key-label` - 返回当前平台对应的 Alt/Option 修饰键标签

### 辅助函数
- `%keyword-value` - 从关键字参数列表中按键取值

### Git 分支探测与监听
- `detect-git-branch` - 检测当前工作区所在的分支名
- `detect-git-dir` - 检测当前工作区的 .git 目录路径
- `git-branch-from-head-file` - 直接从 HEAD 文件解析分支名
- `make-branch-watcher` - 创建分支切换监听器
- `stop-branch-watcher!` - 停止分支监听器

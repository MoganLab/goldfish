# (liii pi code core extensions) 扩展系统聚合测试

`(liii pi code core extensions)` 是 Pi 扩展系统的聚合入口库，重导出注册表、事件、发现、加载等子库接口。

本目录存放跨子库的扩展系统集成测试（如工具调用阻断与终止、工具结果事件派发），各子库的单函数测试位于对应子目录。

使用时导入：

```scheme
(import (liii pi code core extensions))
```

## 子库导览

- `types/` - 扩展注册表、API 与上下文等数据类型定义
- `registry/` - 扩展注册表操作与各类注册入口
- `event/` - 扩展事件监听与派发
- `discovery/` - 扩展目录扫描与发现
- `loader/` - 扩展脚本加载
- `install/` - 扩展安装

# (liii pi code core extensions types core) 模块函数分类索引

`(liii pi code core extensions types core)` 提供扩展注册表、扩展 API 与扩展上下文三种记录类型及其访问器。

使用时导入：

```scheme
(import (liii pi code core extensions types core))
```

## 查看函数文档

```bash
gf doc make-extension-registry
gf doc make-extension-api
gf doc make-extension-context
```

## 函数分类索引

### 扩展注册表
- `make-extension-registry` - 创建空的扩展注册表记录
- `extension-registry?` - 判断对象是否为扩展注册表记录
- `extension-registry-tools` - 读取扩展注册表中的工具定义列表
- `extension-registry-flags` - 读取扩展注册表中的命令行标志定义列表
- `extension-registry-commands` - 读取扩展注册表中的命令定义列表
- `extension-registry-listeners` - 读取扩展注册表中的事件监听器列表
- `extension-registry-flows` - 读取扩展注册表中的流程定义列表
- `extension-registry-validators` - 读取扩展注册表中的参数校验器列表
- `extension-registry-augmenters` - 读取扩展注册表中的配置增强器列表
- `extension-registry-prompt-injectors` - 读取扩展注册表中的提示词注入器列表
- `set-extension-registry-tools!` - 更新扩展注册表中的工具定义列表
- `set-extension-registry-flags!` - 更新扩展注册表中的命令行标志定义列表
- `set-extension-registry-commands!` - 更新扩展注册表中的命令定义列表
- `set-extension-registry-listeners!` - 更新扩展注册表中的事件监听器列表
- `set-extension-registry-flows!` - 更新扩展注册表中的流程定义列表
- `set-extension-registry-validators!` - 更新扩展注册表中的参数校验器列表
- `set-extension-registry-augmenters!` - 更新扩展注册表中的配置增强器列表
- `set-extension-registry-prompt-injectors!` - 更新扩展注册表中的提示词注入器列表

### 扩展 API
- `make-extension-api` - 创建扩展宿主 API 实例
- `extension-api?` - 判断对象是否为扩展 API 记录
- `extension-api-registry` - 读取扩展 API 绑定的注册表记录

### 扩展上下文
- `make-extension-context` - 创建扩展上下文记录
- `extension-context?` - 判断对象是否为扩展上下文记录
- `extension-context-session-id` - 读取扩展上下文中的会话标识
- `extension-context-cwd` - 读取扩展上下文中的工作目录
- `extension-context-model` - 读取扩展上下文中的模型标识
- `extension-context-registry` - 读取扩展上下文绑定的注册表记录
- `extension-context-extra` - 读取扩展上下文中的附加数据

# (liii tui editor paste) 模块函数索引

`(liii tui editor paste)` 提供 bracketed paste 识别、换行/制表符规范化、大段粘贴折叠为占位标记（[paste #N ...]）以及提交时的递归展开恢复功能。

使用时导入：

```scheme
(import (liii tui editor paste))
```

## 查看函数文档

使用以下命令查看函数文档和测试用例：

```bash
gf doc make-paste-registry
```

## 函数分类索引

### 粘贴转义识别与归一化
- `%bracketed-paste?` - 判定是否为 bracketed paste 转义序列
- `%bracketed-paste-content` - 提取纯文本内容
- `%paste-normalize` - 归一化换行、Tab 与控制字符
- `%paste-large?` - 判定是否为大段粘贴

### 粘贴注册表管理
- `make-paste-registry` - 创建粘贴注册表实例
- `paste-registry-add!` - 登记大段文本并生成标记
- `paste-registry-count` - 获取当前登记数
- `paste-registry-remove!` - 删除标记项
- `paste-registry-clear!` - 清空注册表
- `paste-registry-snapshot` - 捕获状态快照
- `paste-registry-restore!` - 从快照恢复

### 占位标记与展开
- `paste-expand` - 展开文本中的所有占位标记
- `paste-marker-at-start` - 检测位置是否为标记开头
- `paste-marker-at-end` - 检测位置是否为标记结尾

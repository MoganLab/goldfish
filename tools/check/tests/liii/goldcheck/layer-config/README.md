# (liii goldcheck layer-config) 模块函数分类索引

分层规则配置解析与模型访问接口，支持解析 JSON 分层配置并提供领域（domain）与分层（layer）的访问器函数。

## 查看函数文档

使用以下命令查看函数文档和测试用例：
```bash
bin/gf doc "函数名"
```

## 函数分类索引

### 一、配置解析与规则判定
- `layer-domains-parse` - 解析配置中的 domains 定义列表并进行合规校验
- `layer-order-enabled?` - 判断配置中是否启用了分层依赖检查规则
- `find-domain-by-name` - 按名称在已解析的域定义列表中查找特定域
- `domain-allows-dep?` - 判断指定域是否允许依赖目标库所属的域

### 二、域对象属性访问
- `domain-name-of` - 获取域的名称
- `domain-root-of` - 获取域的根目录相对路径
- `domain-deps-of` - 获取域显式声明的依赖域名 vector
- `domain-layers-of` - 获取域定义的分层结构列表

### 三、层对象属性访问
- `layer-name-of` - 获取分层的名称
- `layer-libs-of` - 获取分层包含的库名匹配 pattern vector

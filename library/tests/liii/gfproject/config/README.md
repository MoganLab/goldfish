# (liii gfproject config) 模块函数分类索引

`(liii gfproject config)` 负责定位并读取项目配置文件（gfproject.json），并把标准库配置与本地配置深度合并。

## 查看函数文档

```bash
gf doc liii/gfproject/config
```

或者查看具体函数的文档和用例：

```bash
gf doc liii/gfproject/config "gfproject-load-config"
```

## 函数分类索引

### 一、路径定位
- `gfproject-root` - 返回项目根目录路径
- `gfproject-get-gf-lib` - 返回 Goldfish 标准库目录路径
- `gfproject--opt-gf-lib` - 从可选参数中解析标准库目录
- `gfproject-find-local-path` - 查找项目根目录下的 gfproject.json
- `gfproject-find-lib-path` - 查找标准库目录中的 gfproject.json

### 二、配置读取
- `gfproject-read-file` - 读取 JSON 配置文件内容
- `gfproject-extract-tools` - 从配置对象中提取 tools 段
- `gfproject-deep-merge` - 深度合并两个配置对象

### 三、配置加载
- `gfproject-load-config` - 加载并返回合并后的配置
- `gfproject-load-config-bundle` - 加载并返回合并过程的配置包
- `bundle-ref` - 从配置包 alist 中按键取值

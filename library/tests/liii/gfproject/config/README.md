# (liii gfproject config) 模块函数分类索引

`(liii gfproject config)` 负责定位并读取三份配置（内置层、用户层、项目层），并把它们的 `tools` 深度合并。

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
- `gfproject-root` - 返回当前项目根目录路径（不受用户层配置影响）
- `gfproject-get-gf-lib` - 返回 Goldfish 标准库目录路径
- `gfproject--opt-gf-lib` - 从可选参数中解析标准库目录
- `gfproject-find-local-path` - 查找项目根目录下的 gfproject.json
- `gfproject-find-lib-path` - 查找标准库目录中的 gfproject.json
- `gfproject-find-user-path` - 查找用户全局配置 `~/.gf/gfproject.json`

### 二、配置读取
- `gfproject-read-file` - 读取 JSON 配置文件内容
- `gfproject-extract-tools` - 从配置对象中提取 tools 段
- `gfproject-deep-merge` - 深度合并两个配置对象

### 三、配置加载
- `gfproject-load-config` - 加载并返回合并后的配置
- `gfproject-load-config-bundle` - 加载并返回合并过程的配置包
- `bundle-ref` - 从配置包 alist 中按键取值

## 配置包键说明

| 键 | 含义 |
| --- | --- |
| `lib_config` / `lib_path` / `lib_tools` | 内置层原始配置、路径与 tools |
| `user_config` / `user_path` / `user_tools` | 用户层原始配置、路径与 tools |
| `local_config` / `local_path` / `local_tools` | 项目层原始配置、路径与 tools |
| `merged_tools` | `lib -> user -> local` 合并后的 tools，用于命令解析 |
| `merged_config` | 用于展示/兼容的合并配置 |
| `merged_domains` | 仅合并 `lib + local` 的 domains，**不得**用于 load-path |
| `user_domains` | 用户层 domains，单独暴露；用户层 root 相对 `~/.gf` |

`domains` 不跨层合并，因为各层 `root` 的基准目录不同。外部项目的 domains 在运行工具时由
`(liii gfproject external)` 读取其自身 `gfproject.json` 后注入 `*load-path*`。

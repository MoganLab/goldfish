# (liii pi code core skills frontmatter) YAML Frontmatter 解析测试导览

`(liii pi code core skills frontmatter)` 解析 Markdown 文件顶部的
YAML frontmatter（`---` 分隔），支持键值对、引号剥除、块标量（`|`/`>`）
与缩进续行，`disable-model-invocation` 自动转换为布尔值。

## 模块组成

- `tools/pi/liii/pi/code/core/skills/frontmatter.scm` — 解析与剥离实现

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/skills/frontmatter/
```

## 测试文件索引

- `parse-frontmatter-test.scm` — `parse-frontmatter`：元数据/正文切分、布尔转换、引号剥除、多行描述与容错
- `strip-frontmatter-test.scm` — `strip-frontmatter`：仅取正文

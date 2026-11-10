# (liii pi code core skills types) 技能记录与规范校验测试导览

`(liii pi code core skills types)` 定义 Agent Skills 的核心数据结构
`<pi-skill>` 与规范校验：name 字符集/长度/短横线规则、description 必填
与长度上限（对齐 agentskills.io 规范与上游 pi agent）。

## 模块组成

- `tools/pi/liii/pi/code/core/skills/types.scm` — 记录定义与校验函数

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/skills/types/
```

## 测试文件索引

- `%make-pi-skill-test.scm` — `%make-pi-skill`：记录构造
- `pi-skill-p-test.scm` — `pi-skill?`：类型谓词
- `pi-skill-name-test.scm` — `pi-skill-name`：名称读取
- `pi-skill-description-test.scm` — `pi-skill-description`：描述读取
- `pi-skill-file-path-test.scm` — `pi-skill-file-path`：SKILL.md 路径读取
- `pi-skill-base-dir-test.scm` — `pi-skill-base-dir`：技能根目录读取
- `pi-skill-disable-model-invocation-p-test.scm` — `pi-skill-disable-model-invocation?`：禁用标志读取
- `pi-validate-skill-name-test.scm` — `pi-validate-skill-name`：名称规范校验（字符集、长度、首尾与连续短横线）
- `pi-validate-skill-description-test.scm` — `pi-validate-skill-description`：描述必填与长度校验

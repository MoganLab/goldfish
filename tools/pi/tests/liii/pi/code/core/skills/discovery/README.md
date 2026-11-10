# (liii pi code core skills discovery) 技能目录发现测试导览

`(liii pi code core skills discovery)` 递归扫描技能目录：默认目录按
「gf 优先、pi 兼容」合并（`$GF_CONFIG_DIR/skills` → `~/.gf/agent/skills`
优先，`$PI_CODING_AGENT_DIR/skills` → `~/.pi/agent/skills` 兼容，项目级
仅查当前目录 `.pi/skills`），另支持 `--skill` 显式路径；SKILL.md 边界
停止递归、跳过隐藏目录与 `node_modules`、未写 name 时回退父目录名、
同名冲突首见胜出并输出 collision 警告。

## 模块组成

- `tools/pi/liii/pi/code/core/skills/discovery.scm` — 目录发现与去重实现

## 运行测试

```bash
bin/gf test tools/pi/tests/liii/pi/code/core/skills/discovery/
```

## 测试文件索引

- `pi-load-skills-test.scm` — `pi-load-skills`：多源合并、边界保护、隐藏目录/node_modules 跳过、父目录名回退与 Winner/Loser 冲突去重

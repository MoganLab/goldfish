# Goldfish Scheme AUR 打包指南

本目录包含 Goldfish Scheme 针对 Arch Linux 用户软件仓库 (AUR, Arch User Repository) 的打包配置。

> **说明**：AUR 上已存在同名的无关工具 `goldfish`（一个文件搜索工具），因此本项目的 AUR 包名统一命名为 `goldfish-scheme`，并提供 `goldfish` 别名。

## 目录结构

- `goldfish-scheme/`: 稳定版本包（基于发布的 Release Tag）
- `goldfish-scheme-git/`: 开发滚动版本包（追踪 Git 仓库最新提交）

每个目录下均包含：
- `PKGBUILD`: Arch Linux 软件包构建定义文件
- `.SRCINFO`: AUR 依赖与元数据索引（由 `makepkg --printsrcinfo > .SRCINFO` 生成）

## 1. 本地测试构建

在 Arch Linux / CachyOS / Manjaro 等基于 Arch 的系统中，可以通过以下命令进行本地测试与安装：

### 构建稳定版包

```bash
cd packaging/aur/goldfish-scheme
makepkg -si
```

### 构建 Git 开发版包

```bash
cd packaging/aur/goldfish-scheme-git
makepkg -si
```

> **提示**：如果系统上的 `xmake` 是通过脚本独立安装在 `~/.local/bin` 而非系统包管理器，构建时若提示依赖缺失，可加上 `-d` 参数跳过依赖检查：`makepkg -d -f`。

## 2. 首次发布到 AUR

### 2.1 准备工作

1. 在 [AUR](https://aur.archlinux.org/) 注册账号并上传 SSH 公钥。
2. 确保本地能够通过 SSH 连接 AUR：
   ```bash
   ssh -T aur@aur.archlinux.org
   ```

### 2.2 发布 `goldfish-scheme`

```bash
# 1. 克隆对应的 AUR 空仓库
git clone ssh://aur@aur.archlinux.org/goldfish-scheme.git /tmp/aur-goldfish-scheme

# 2. 将打包文件复制到仓库中
cp packaging/aur/goldfish-scheme/PKGBUILD /tmp/aur-goldfish-scheme/
cd /tmp/aur-goldfish-scheme

# 3. 校验并重新生成 .SRCINFO
makepkg --printsrcinfo > .SRCINFO

# 4. 提交并推送到 AUR
git add PKGBUILD .SRCINFO
git commit -m "Initial release v18.11.60"
git push origin master
```

### 2.3 发布 `goldfish-scheme-git`

```bash
# 1. 克隆对应的 AUR 空仓库
git clone ssh://aur@aur.archlinux.org/goldfish-scheme-git.git /tmp/aur-goldfish-scheme-git

# 2. 将打包文件复制到仓库中
cp packaging/aur/goldfish-scheme-git/PKGBUILD /tmp/aur-goldfish-scheme-git/
cd /tmp/aur-goldfish-scheme-git

# 3. 校验并重新生成 .SRCINFO
makepkg --printsrcinfo > .SRCINFO

# 4. 提交并推送到 AUR
git add PKGBUILD .SRCINFO
git commit -m "Initial release (git version)"
git push origin master
```

## 3. 日常版本更新维护

当 Goldfish 发布新版本（例如 `v18.11.61`）时：

1. 修改 `packaging/aur/goldfish-scheme/PKGBUILD` 中的 `pkgver` 和 `pkgrel=1`。
2. 更新源码校验和（sha256sums）：
   ```bash
   updpkgsums PKGBUILD
   # 或者手动获取校验和并填入
   ```
3. 重新生成 `.SRCINFO`：
   ```bash
   makepkg --printsrcinfo > .SRCINFO
   ```
4. 将更新同步至 AUR 仓库并 `git push`。

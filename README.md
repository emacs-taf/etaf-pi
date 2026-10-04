# ETAF Pi

在 Emacs 里原生使用 Pi。ETAF 显示流式对话、工具状态和用量，
Emacs 负责文字编辑。不是内嵌终端，也不抓取 Pi 的屏幕输出。

## 使用

需要 Emacs 29.1+、PATH 中的 Pi（开发所用协议版本 0.87.1）和
`etaf`、`etaf-ui`、`ebox`、`ecss` 的检出。提供者可放在应用旁边，
也可沿用 `emacs-taf/etaf-apps/etaf-pi` 的嵌套工作区。

把这些仓库的根目录加入 `load-path` 后：

```elisp
(require 'etaf-pi)
(etaf-pi-open)
```

`M-x etaf-pi-open` 按当前项目打开或切回会话；`C-u M-x etaf-pi-open`
新开一个。每个客户端有独立 Pi 子进程，沿用 Pi 的模型、认证、Skills、
提示词、扩展和会话文件，不创建第二份聊天数据库。

| 能力 | 界面位置 | 按键 |
|---|---|---|
| 多行消息与代码 | 对话底部的输入区（就在应用界面内原地编辑） | `i` / `e` 聚焦，Enter 发送、Shift+Enter 换行 |
| 快速命令 | 原生 minibuffer | `:`，TAB 补全；输入区用 `M-TAB` |
| 工具/扩展确认 | 状态栏提示、原生选择器 | `a`，`C-g` 取消 |
| 停止、收回待发文本 | 工具栏 | `C-c C-k` |
| 翻历史 / 跟随最新 | 对话区 | 滚轮、`j/k`、PageDown/PageUp，`g` |
| 展开完整工具输出 | 可搜索的只读 buffer | `t` |
| 模型 / 思考级别 | 状态栏、原生选择器 | `m` / `T` |
| 新会话 / 恢复 | Pi 原有会话文件 | `n` / `r` |
| 断线重连 / 帮助 / 关闭 | 当前客户端 | `R` / `?` / `q` |

界面采用会话侧栏、消息流和底部输入区；模型选择在输入区下沿，发送与停止靠右。
窄窗口自动收起侧栏。`Ctrl+Enter`、`C-c C-c` 也可发送，`C-j` 也可换行；输入区用 `M-TAB` 补全 Slash 命令。

输入区编辑的就是对话 buffer 里那段真实文本：没有额外窗口、编辑器 buffer 或 frame，
输入法、选区、撤销和补全都保持原生。提交后文本保留到 Pi 确认接收；确认只清空没有
再次改动的输入，发送失败或等待期间继续输入都不丢字。按 `C-c C-k` / `Esc` 收起输入区
并保留草稿，之后按 `i` 或点回输入区继续。

Slash 命令：`/new /resume /model /thinking /abort /compact /quit /help`。
Skills、模板与扩展命令从 Pi 的 `get_commands` 发现，TAB 可补全。
任务运行时的新消息排入 follow-up 队列，不丢弃。确认不会抢走其他 buffer
的焦点，也不会自动批准。状态栏提示后按 `a` 处理。

## 边界

- 有界文本视口：流式刷新只更新可见对话 Host，历史通过滚轮和翻页浏览。
- 支持 Markdown 标题、粗体与代码块，代码保持原文；不是完整 Markdown/HTML 浏览器。
- 暂不显示图片、会话树和终端专用扩展 UI。Pi 的 `custom()`、TUI 页脚、
  主题、替换输入编辑器等在 RPC 本身就不可用。
- 恢复选择器目前使用 Emacs 原生文件选择，从当前会话目录开始。
- 不要同时在终端和客户端写同一个会话文件。客户端内部会拒绝重复打开；
  外部 Pi 进程的占用由使用者协调。

详见 [使用手册](docs/manual.md)、[通信与数据契约](docs/architecture.md)。
公共 Lisp API 以 [根入口 Commentary](etaf-pi.el) 为准。

## 开发与检查

```sh
make check                     # 结构、编译、离线进程契约、公共 API
make setup-hooks
```

测试只运行 Python 假进程，回放 `tests/fixtures/pi-stream.jsonl` 中的真实
Pi 事件格式，不连网络、不需要 Pi、也不消费模型额度。明确需要重新录制时：

```sh
python3 scripts/record-fixture.py --with-model --output /tmp/pi-stream.jsonl
```

性能场景在 `benchmarks/performance-scenarios.el`，由上层工作区
`scripts/performance.py` 驱动。GUI 场景在 `scripts/etaf-pi-gui-scenarios.el`。

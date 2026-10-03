# 使用手册

## 项目与会话

在项目文件里执行 `M-x etaf-pi-open`，客户端使用 `project.el` 的项目根。
同一项目再次执行会切回原 buffer。加 `C-u` 可在同一项目另开一个独立会话。
非项目 buffer 沿用 `default-directory`。Lisp 调用可明确传入目录：

```elisp
(etaf-pi-open "/path/to/project")
```

`n` 或 `/new` 让 Pi 新建会话，不删除旧记录。`r` 或 `/resume` 从 Pi 的原有
JSONL 文件恢复。切换只能在空闲时做，运行中先 `C-c C-k` 停止。
退出后也可用终端里的 Pi 恢复，但不能让两个进程同时写同一会话。
切换会话期间仍可编辑草稿，但暂不发送，避免请求落入旧会话。
再次打开一个已有客户端会复用其窗格；切换客户端会收起旧界面，保留它的后台与草稿。

## 输入与工具

`M-x etaf-pi-open` 打开会话和底部输入区。对话区按 `i` 或 `e` 回到输入。
Enter、`C-c C-c`、Ctrl+Enter 或右下方发送按钮发送；Shift+Enter / `C-j` 换行。
模型与思考选项在输入框下沿，选完回到输入；运行中出现停止按钮。发送后输入区保持打开；
Pi 确认接收才清空原文，失败或发送期间继续编辑时不清空。
`C-c C-k` 返回对话区并保留草稿，之后按 `i` 恢复。
换行、选区、撤销与输入法都由 Emacs 原生编辑负责。

输入区 `M-TAB` 补全 Slash 命令；对话区按 `:` 打开带历史的快速命令输入。
滚轮每次移动三行，`j/k`、PageDown/PageUp、`C-v/M-v` 翻页。
查看历史时不随新回复跳动，按 `g` 或「最新」恢复跟随。

任务运行中新输入默认排队等待当前任务结束。停止先清空队列，再中断；
被收回的队列文本会放到下一次输入的草稿，不自动重发。

工具结果在对话中显示摘要，`t` 查看完整原文，用 Emacs 搜索和复制。
工具确认或扩展提问出现在状态栏，按 `a` 才打开选择器。`C-g` 拒绝本次
请求；它从不默认批准。请求过期后不再发送回答。

## 配置与扩展

模型与思考级别分别用 `m`、`T`，选项来自运行中的 Pi。Slash 命令除了
客户端明确实现的内置命令，还包括 `get_commands` 返回的 Skills、模板
和扩展命令。终端专用 `/settings` 之类不会作为普通文本发给模型。

Pi 在 RPC 模式会正常加载资源，但终端专用扩展接口不支持或降级。
特别是 `ctx.ui.custom()`、终端输入钩子、页脚和主题替换不能在这里复刻。
标准 `select/confirm/input/editor/notify` 使用 RPC 的 UI 子协议。

## 错误与恢复

- 启动失败：界面显示错误，确认 Emacs 的 `exec-path` 能找到 `pi` 和 Node；
  修复后按 `R`，不需要重启 Emacs。
- 进程退出：保留当前显示；按 `R` 用最后已知会话重新连接。
- 超时：显示结果未知，不自动重发写入；先确认 Pi 的实际状态。
- 后端模型错误：原样显示错误文字；模型失败不等于客户端离线。
- 显示错误：保留错误提示，不静默报告成功。
- 关闭：释放此 buffer 的 timer、runtime 和子进程，不碰其他会话或用户 Emacs。

## Lisp 集成

根入口的 Commentary 是 API 清单。传输可以单独创建并订阅事件：

```elisp
(let ((connection
       (etaf-pi-rpc-start default-directory
                          (lambda (event) (message "%S" event)))))
  (etaf-pi-rpc-request connection "get_state" nil
                       (lambda (reply)
                         (message "%S" reply)
                         (etaf-pi-rpc-close connection))))
```

异步代码需要词法绑定（`lexical-binding: t`）。事件回调收到 plist，
应投影状态而非打开 modal 输入；模态输入由显式用户命令完成。

## 性能与 GUI 验收

使用上层工作区的统一性能入口，测量独立后台 GUI 的局部文本更新：

```sh
python3 scripts/performance.py run --mode gui --server etaf-pi-verify \
  --scenario-file etaf-apps/etaf-pi/benchmarks/performance-scenarios.el \
  --load-path etaf-apps/etaf-pi \
  --scenario pi.client.history --samples 30 --warmups 5 --sets 3
```

`pi.client.history` 测真实客户端的带样式长对话翻页，包含 redisplay 和 GC；
`pi.stream.retained` 是独立的底层文本更新参照，不能代替应用验收。
结果记录当时的焦点条件，不是屏幕合成延迟。
GUI 自动场景使用真实 Emacs 控件激活和命令循环，覆盖三种宽度和矮高；
离线协议回放不替代真实后端验收。截图与日志留在临时/报告目录，不入库。

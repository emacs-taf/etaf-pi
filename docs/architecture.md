# Architecture and wire contract

## Ownership

Pi is the only authority for sessions, tools, authentication, prompts, context
compaction and model usage. The client starts one `pi --mode rpc` per buffer
with its project as cwd. It never edits session files or copies authentication.
The service module owns processes/JSONL, the model projects events without I/O,
views consume values, and the app owns commands, timers and buffers. ETAF and
ETAF UI are accessed through their public entries only.

There is no application-owned domain database. User drafts are ephemeral Emacs
buffers/strings. Pi saves the conversation using its normal JSONL store.

## Transport contract (version 2)

Upstream references: the installed Pi package's `docs/rpc.md`,
`docs/rpc-commands.md`, `docs/json.md`, `docs/message-types.md` and
`docs/rpc-extension-ui.md`. This adapter was developed against Pi 0.87.1.
Changing adapter semantics requires updating this version and its tests.

- argv is executed without a shell; stdin/stdout use UTF-8 JSON records delimited
  only by LF, accepting CRLF. U+2028 and U+2029 inside strings are not separators.
- Each command has a unique id. Responses are correlated by id, never position.
  Synthetic failure replies include the original command name. All records pass
  through one FIFO, including when callbacks yield and reenter the process filter.
  Callback errors or quits do not drop remaining records.
  Unsolicited events use a separate callback. Stdout must stay protocol-only;
  stderr is diagnostic text, retained as a bounded 64 KiB tail.
- Requests time out once without retrying writes. A timeout means outcome unknown,
  not rollback. Malformed JSON closes the connection; unknown valid events are
  tolerated. EOF or unexpected process exit resolves outstanding requests.
- `prompt` success is acceptance, not completion. `agent_end` can precede retry;
  only `agent_settled` settles the run. Streaming deltas are indexed by content
  block, final block/message records replace the provisional display.
- `extension_ui_response` is a separate subprotocol, reusing the request's id.
  Explicit false stays JSON false. Confirmation is never inferred or automated.
- Close stops scheduling UI updates, requests EOF, and after two seconds kills
  only the owned process if necessary. A killed buffer cannot be revived by late
  events. Restart never re-sends an uncertain prompt.
- The transport inherits Pi's configured environment; normal Pi extensions own
  model routing. The opt-in fixture recorder explicitly selects direct routing
  for its DeepSeek request and disables user resources.

## Rendering and input

The application paints a bounded transcript Host, separate from its status
Host, and an in-layout composer. Navigation has its own ETAF runtime in an
owned native window, so the sidebar keeps the full height of the conversation
area; the conversation, the composer field and its control row share one
Runtime. Message text wraps at the available width (capped at 92 cells);
completed-message wrapping is cached per width. Semantic text faces distinguish
user messages, replies, code and tool summaries without changing row metrics.
Incoming deltas only set dirty state. A single 80 ms timer coalesces them into a
retained content batch. The transcript uses one Host rather than one per line to
avoid repeated projection overhead. Nothing is scheduled while idle. Resize
rebuilds layout, not conversation history. Scrolling stops auto-follow.
Lines borrowed by an active minibuffer are not a resize: the committed rows
keep the layout until the prompt closes.

The composer is ETAF UI's `etaf-text-input` inside that surface: activation
opens a native session over the field's published text in the conversation
buffer, so native Emacs owns editing, IME, point, selection, undo and
completion, and no extra frame or editor buffer exists. The framework defers
publication while the session owns the text and republishes the accepted value
when it ends; per-keystroke edits reach the draft through the field's binding.
Pi owns the command map, draft mirroring and send/acknowledgement policy. Enter
sends, Shift+Return inserts a newline, and Escape/`C-c C-k` ends the session
while keeping the accepted text as the draft. Acceptance clears the draft only
when no session is active and the draft still equals the submitted text, so a
failed request or any intervening edit preserves it. Because a session edits
the surface's real text, the paint after it ends rebuilds the view instead of
patching retained cohorts. No editable region is shared across runtimes.
Asynchronous extension dialogs enter an explicit
queue; pressing `a` opens native input. A supplied backend timeout is checked
before answering. C-g sends cancellation, never confirmation.

## Offline evidence and drift

`tests/fixtures/pi-stream.jsonl` was recorded from a real Pi 0.87.1 process using
DeepSeek Flash with no tools, user extensions, skills or context files. System
documentation/cwd paths are normalized in the checked-in sample; event shapes,
content block indexes and message lifecycle are otherwise retained.
`fake-pi.py` replays these events and adds deterministic tool/dialog/failure cases,
fragmenting UTF-8 records across writes. Contract tests require neither Pi nor
network access. Manual backend verification is separate from this regression gate.

The GUI remains usable after an error; `R` reconnects a dead backend, and `q`
closes it. Unsupported TUI-only commands are rejected locally instead of being
silently sent to the model as conversation text.

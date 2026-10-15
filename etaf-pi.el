;;; etaf-pi.el --- Native Pi client for ETAF -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: ETAF contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (etaf "0.2.1") (etaf-ui "0.1.0"))
;; Keywords: tools, convenience
;; URL: https://github.com/emacs-taf/etaf-pi
;;; Commentary:
;; Public API
;; Pi owns conversations, authentication, tools and persistence.  ETAF owns
;; presentation.  The asynchronous transport is also usable without a View.
;; Only the interfaces below are supported; require this root entry.
;;
;; Function: etaf-pi-rpc-start (directory listener &optional command)
;; Start a JSONL child in DIRECTORY. LISTENER receives event plists; COMMAND
;; optionally supplies argv for a compatible backend. Return an opaque connection.
;; Function: etaf-pi-rpc-request (connection type &optional fields callback timeout)
;; Send TYPE/FIELDS; return request id, deliver response to CALLBACK once.
;; TIMEOUT seconds defaults to 30; no retries or implied rollback on timeout.
;; Function: etaf-pi-rpc-respond (connection id fields)
;; Answer one extension dialog with :value, :confirmed or :cancelled.
;; Function: etaf-pi-rpc-info (connection)
;; Query liveness, PID, pending count, error and diagnostic stderr tail.
;; Function: etaf-pi-rpc-close (connection)
;; Idempotent nonblocking EOF, then terminate only this process after two seconds.
;;
;; Function: etaf-pi-open (&optional directory new)
;;;###autoload (autoload 'etaf-pi-open "etaf-pi" nil t)
;; Open or select a project client; NEW forces a separate session.
;; Function: etaf-pi-close ()
;;;###autoload (autoload 'etaf-pi-close "etaf-pi" nil t)
;; Close current client and its connection, keeping Pi's persisted session.
;;
;; Function: etaf-pi-state-create ()
;; Return a fresh headless protocol projection.
;; Function: etaf-pi-state-accept (state record)
;; Consume one parsed wire record, returning STATE. No I/O or rendering.
;; Function: etaf-pi-state-messages (state)
;; Return copied final messages plus the reconstructed streaming draft.
;; Function: etaf-pi-state-info (state)
;; Return copied status, session, usage, queue and pending-dialog facts.
;; Function: etaf-pi-info ()
;; Return client facts and connection liveness without network I/O. :composer
;; describes the in-layout field: its reference, whether a native editing
;; session is active, the current text, any editor dialog and sending state.
;; :navigation names the visible sidebar buffer. :session-changing
;; gates sends during a switch; :render-pending and :controls-phase describe
;; whether the visible controls have caught up with backend state.
;; Function: etaf-pi-send (text &optional behavior)
;; Send TEXT; while running, BEHAVIOR defaults to followUp (or use steer).
;; Function: etaf-pi-compose (text)
;; Read TEXT through native minibuffer editing and send it.
;; Function: etaf-pi-refresh ()
;; Asynchronously read current state, usage and messages from Pi.
;; Function: etaf-pi-reconnect ()
;; Restart a dead backend, passing its last known session to Pi for recovery.
;; Refuse to start a second process while the connection is alive.
;; Function: etaf-pi-abort ()
;; Clear queued prompts before aborting; retain their text as a draft.
;; Function: etaf-pi-new-session ()
;; Start a fresh session only when idle. Previous saved files remain untouched.
;; Function: etaf-pi-resume (file)
;; Ask Pi to resume FILE; reject another client already owning that file.
;; Function: etaf-pi-select-model (&optional name)
;; Select provider/id NAME or prompt from the backend's model list when nil.
;; Function: etaf-pi-select-thinking (&optional level)
;; Set LEVEL or prompt from backend-supported reasoning levels.
;; Function: etaf-pi-answer (&optional value cancel)
;; Answer the next dialog with VALUE, or CANCEL it. Interactive input asks the
;; user; noninteractive confirm requires a non-nil VALUE to approve.
;; Function: etaf-pi-edit (&optional text dialog)
;; Focus the in-layout composer, optionally prefilled with TEXT. DIALOG marks a
;; Pi editor request answered by the next submission.
;; Function: etaf-pi-page-up (&optional lines)
;; Browse older transcript lines without following incoming output. LINES defaults
;; to a viewport; the wheel passes three for smaller steps.
;; Function: etaf-pi-page-down (&optional lines)
;; Browse newer LINES (default a viewport), resuming follow at the transcript's end.
;; Function: etaf-pi-follow ()
;; Display and follow the latest output.
;; Function: etaf-pi-show-tools ()
;; Open full authoritative tool output in a native read-only buffer.
;; Function: etaf-pi-help ()
;; Display read-only keyboard help and RPC limitations in the current client.
;; Return its buffer, without opening another buffer or splitting a window.
;; Escape or an outside click closes help and restores the opening focus.
;; Requires a mounted Pi client; viewing does not send prompts or approvals.
;; Variable: etaf-pi-command
;; Child argv. Default is ("pi" "--mode" "rpc"); may be dynamically bound for
;; offline protocol peers. Pi loads normal user resources unless flags say otherwise.
;; Variable: etaf-pi-refresh-interval
;; Minimum coalesced repaint interval in seconds; defaults to 0.08.
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory
                                               (or load-file-name buffer-file-name))))
(require 'etaf-pi-rpc)
(require 'etaf-pi-app)
(provide 'etaf-pi)
;;; etaf-pi.el ends here

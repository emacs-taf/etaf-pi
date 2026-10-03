;;; performance-scenarios.el --- Streaming publication workload -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Background key/callback-to-redisplay measurements; not compositor latency.
;;; Code:
(require 'etaf)
(declare-function taf-benchmark-register "performance-driver" (id &rest properties))
(taf-benchmark-register
 "pi.stream.retained" :version 1 :modes '(batch gui) :budget-ms 30
 :impact 'local :isolation 'series
 :description "Replace streaming prose through the public retained Host boundary"
 :setup
 (lambda (_mode)
   (let* ((buffer (generate-new-buffer " *Pi stream benchmark*"))
          (text (mapconcat (lambda (n) (format "第 %d 行：ETAF 原生客户端逐段显示回复。" n))
                           (number-sequence 1 20) "\n")))
     (etaf-mount buffer
                 (etaf-view
                  (column :width 'stretch
                    (text "Pi streaming")
                    (box :width 'stretch :wrap-mode 'word
                      (text :id 'pi-stream :ref 'pi-stream (expr text)))))
                 '(:viewport-width 900 :viewport-height 40))
     (list :buffer buffer
           :invoke (lambda (sample)
                     (etaf-runtime-update-host-content
                      (etaf-runtime-for-buffer buffer) 'pi-stream
                      (concat text (format "\n更新 %d" sample)))
                     (redisplay t))
           :verify (lambda ()
                     (with-current-buffer buffer
                       (if (string-match-p "更新" (buffer-substring-no-properties
                                                  (point-min) (point-max)))
                           t "stream text missing")))
           :cleanup (lambda ()
                      (when (buffer-live-p buffer)
                        (etaf-unmount (etaf-runtime-for-buffer buffer))
                        (kill-buffer buffer)))))))
(defconst etaf-pi-benchmark--root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(taf-benchmark-register
 "pi.client.history" :version 1 :modes '(batch gui) :budget-ms 100
 :impact 'local :isolation 'series
 :description "Scroll the real client with styled message/code/tool rows"
 :setup
 (lambda (_mode)
   (require 'etaf-pi)
   (let* ((etaf-pi-command (list "python3" (expand-file-name "tests/fixtures/fake-pi.py" etaf-pi-benchmark--root)))
          (buffer (etaf-pi-open etaf-pi-benchmark--root t)))
     (with-current-buffer buffer
       (dotimes (turn 3)
         (let ((deadline (+ (float-time) 5)))
           (while (and (or (not (equal (plist-get (etaf-pi-info) :phase) "就绪"))
                            (> (plist-get (plist-get (etaf-pi-info) :connection) :pending) 0))
                       (< (float-time) deadline)) (sleep-for .02)))
         (etaf-pi-send (format "第 %d 轮排版，检查消息与代码。" turn))
         (sleep-for .5))
       (etaf-pi-follow))
     (list :buffer buffer
           :invoke (lambda (sample)
                     (with-current-buffer buffer
                       (if (= (% sample 2) 0) (etaf-pi-page-up) (etaf-pi-page-down))
                       (redisplay t)))
           :verify (lambda ()
                     (with-current-buffer buffer
                       (if (and (not (plist-get (etaf-pi-info) :error))
                                (> (plist-get (etaf-pi-info) :total-lines) 40))
                           t "Client history not ready")))
           :cleanup (lambda ()
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer (etaf-pi-close))))))))
(provide 'performance-scenarios)
;;; performance-scenarios.el ends here

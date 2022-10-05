;;; quarto-mode.el --- A (poly)mode for https://quarto.org -*- lexical-binding: t -*-
;;
;; Author: Carlos Scheidegger
;; Maintainer: Carlos Scheidegger
;; Copyright (C) 2022 RStudio PBC
;; Version: 0.0.3
;; package-requires: ((emacs "25.1") (polymode "0.2.2") (poly-markdown "0.2.2") (markdown-mode "2.3") (request "0.3.2"))
;; URL: https://github.com/quarto-dev/quarto-emacs
;; Keywords: languages, multi-modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode)
(require 'markdown-mode)
(require 'poly-markdown)
(require 'shell)
(require 'mode-local)
(require 'request)

;;; Requires ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst quarto-mode--docstring
  "Minor mode for editing quarto files.")

;; this package doesn't require poly-R, ess-mode, or ess-r-mode, but
;; works differently when these are available.
(if (and (require 'ess-mode nil t)
         (require 'ess-r-mode nil t)
         (require 'poly-R nil t))
;;;###autoload (autoload 'poly-quarto-mode "quarto-mode")
    (define-polymode poly-quarto-mode poly-markdown+r-mode
      "Minor mode for editing quarto files."
      :lighter " Quarto"
      :keymap nil
      :innermodes '(:inherit poly-r-markdown-inline-code-innermode))
;;;###autoload (autoload 'poly-quarto-mode "quarto-mode")
  (define-polymode poly-quarto-mode poly-markdown-mode
    "Minor mode for editing quarto files."
    :lighter " Quarto"
    :keymap nil
    :innermodes '(:inherit poly-markdown-inline-code-innermode)))

;;; Customizable variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup quarto nil
  "Settings for the quarto polymode"
  :group 'polymode)

(defcustom quarto-root-polymode
  (pm-polymode :name "R"
               :hostmode 'pm-host/R
               :innermodes '(pm-inner/fundamental))
  "Root polymode with R host intended to be inherited from."
  :group 'quarto
  :type 'object)

(defcustom quarto-poly-markdown-exporter
  (pm-shell-exporter :name "quarto"
		     :from
		     '(("quarto" "\\.qmd" "quarto Markdown"
			"quarto render --to=%t --output=%o"))
		     :to
		     '(("auto" . quarto-pm--shell-auto-selector)
                       ("default" . quarto-pm--shell-auto-selector)
		       ("html" "html" "html document" "html")
                       ("pdf" "pdf" "pdf document" "pdf")
                       ("word" "docx" "word document" "docx")
		       ("revealjs" "html" "revealjs presentation" "revealjs"))) ;; TODO fill this out automatically
  "Quarto Markdown exporter.
Please note that with 'AUTO DETECT' export options, output file
names are inferred by quarto from the appropriate metadata.
That is, output file names don't comply with `polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter quarto-poly-markdown-exporter
			    nil poly-quarto-polymode)

(defcustom quarto-preview-display-buffer t
  "When nil, `quarto-preview' does not automatically display buffer."
  :group 'quarto
  :type 'boolean)

(defcustom quarto-force-preview t
  "When t, all markdown rendering commands go through quarto-preview instead of producing
disk output."
  :group 'quarto
  :type 'boolean)

(defcustom quarto-command (let ((cmd (executable-find "quarto")))
			    (and cmd (file-name-nondirectory cmd)))
  "Command to run quarto."
  :group 'quarto
  :type '(choice (string :tag "Shell command") (repeat (string)) function))

;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quarto-pm--output-file-sniffer ()
  "Detect the output format from a quarto run."
  (goto-char (point-min))
  (let (files)
    (while (re-search-forward "Output created: +\\(.*\\)" nil t)
      (push (expand-file-name (match-string 1)) files))
    (reverse (delete-dups files))))

(defun quarto-pm--shell-auto-selector (action &rest _ignore)
  "Select the output format automatically from a run of quarto in a shell.

  ACTION decides which action to take, `doc`, `command` or `output-file`.

  (This is an internal function.)"
  (cl-case action
    (doc "AUTO DETECT")
    (command "quarto render %i")
    (output-file #'quarto-pm--output-file-sniffer)))

;; FIXME will this work on windows? :shrug:
(defun quarto-mode--parent-directories (file-name)
  "Produce a list of parent directories of FILE-NAME."
  (let* ((dir (file-name-directory file-name))
	 (result '()))
    (while (not (string-equal dir "/"))
      (push dir result)
      (setq dir (file-name-directory
		 (substring dir 0 -1))))
    (push "/" result)
    (reverse result)))
	    
(defun quarto-mode--buffer-in-quarto-project-p ()
  "Return the full filename of the _quarto.yml project configuration for the present buffer or nil if none is found."
  (locate-dominating-file buffer-file-name "_quarto.yml"))

(defvar quarto-mode--preview-process nil)
(defvar quarto-mode--preview-url nil)

(defun quarto-mode--process-line (line)
  "Process line LINE from a `quarto-preview' output."
  (with-current-buffer (get-buffer-create "*debug*")
    (insert line))
  (let ((re-url ".*Browse at \\(http://[.-:A-Z_a-z-]+?\\)$"))
    (cond
     ((string-match re-url line)
      (setq quarto-mode--preview-url
	    (url-generic-parse-url (match-string 1 line)))))))

(defun quarto-mode--process-filter (proc string)
  "Filter the output generated by a `quarto-preview' process.  This is a process filter operating on process PROC and input STRING."
  (with-current-buffer (get-buffer "*quarto-preview*")
    (let ((prev-lines (count-lines (point-min) (point-max))))
      (comint-output-filter proc string)
      (let* ((curr-lines (count-lines (point-min) (point-max)))
	     (this-line prev-lines))
	(save-excursion
	  (goto-char (point-min))
	  (forward-line (- this-line 1))
	  (while (<= this-line curr-lines)
	    (let ((line (buffer-substring (point)
					  (progn (forward-line 1)
						 (point)))))
	      (quarto-mode--process-line line)
	      (setq this-line (+ this-line 1)))))))))

;; we obtained this uuid ourselves on 2022-04-04
(defconst quarto-mode--quarto-preview-uuid "5E89DF46-0E7B-4604-A76A-58EC4E12A11B")

(defun quarto-preview ()
  "Start (or restart) a quarto preview process to automatically
rerender documents.

`quarto-preview` checks parent directories for a `_quarto.yml`
file.  If one is found, then `quarto-preview` previews that entire
project, this is \"project mode\"..

If not, then `quarto-preview` previews the file for the current
buffer, this is \"file mode\".

In project mode, project files aren't automatically watched in
the file system.

To control whether or not to show the display, customize
`quarto-preview-display-buffer`."
  (interactive)
  (when quarto-mode--preview-process
    (delete-process quarto-mode--preview-process))
  (when (get-buffer "*quarto-preview*")
    (kill-buffer "*quarto-preview*"))
    
  (let* ((project-directory (quarto-mode--buffer-in-quarto-project-p))
	 (browser-path (cond
			(project-directory
			 (file-relative-name buffer-file-name project-directory))
			(t "")))
	 (process
	  (let ((process-environment (cons (concat "QUARTO_RENDER_TOKEN="
						   quarto-mode--quarto-preview-uuid)
					   process-environment)))
	    (make-process :name (format "quarto-preview-%s" buffer-file-name)
			  :buffer "*quarto-preview*"
			  :command (list quarto-command
					 "preview"
					 buffer-file-name
					 "--no-watch-inputs")))))
    (setq quarto-mode--preview-process process)
    (with-current-buffer (process-buffer process)
      (when quarto-preview-display-buffer
	(display-buffer (current-buffer)))
      (shell-mode)
      (set-process-filter process 'quarto-mode--process-filter))))

(easy-menu-define quarto-menu
  (list poly-quarto-mode-map)
  "Menu for quarto-mode"
  '("Quarto"
    ["Start Preview" quarto-preview t]))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(cl-defun quarto-mode--preview-refresh-complete
    (&key data response symbol-status error-thrown)
  "Refresh quarto-preview when the status code of the rerender request is not 200.

This is an internal function."
  (unless (= (request-response-status-code response) 200)
    (quarto-preview)))

(defun quarto-mode--maybe-preview (name)
  "Call `quarto-preview' with filename NAME when needed.  In that case, errors intentionally to avoid `markdown-command' from finishing."
  (with-current-buffer (get-file-buffer name)
    (let* ((this-project-directory
	    (quarto-mode--buffer-in-quarto-project-p)))
      ;; only run these when preview is enabled
      (when quarto-force-preview
	(save-buffer)
	(cond
	 ;; no process or exited process: restart
	 ((and quarto-mode--preview-process
	       (member (process-status quarto-mode--preview-process)
		       (list 'exit 'signal nil)))
	  (quarto-preview)
	  (error "Opening %s via quarto-preview" name))
	 ;; re-run quarto-preview when previous quarto-preview server returns non-200
	 (t
	  (let ((req (format "%s://%s:%s/%s?path=%s"
			     (url-type quarto-mode--preview-url)
			     (url-host quarto-mode--preview-url)
			     (url-port quarto-mode--preview-url)
			     quarto-mode--quarto-preview-uuid
			     buffer-file-name)))
	    (request req :sync t
	      :complete #'quarto-mode--preview-refresh-complete)
	    ;; this is not really an error,
	    ;; but it's the best way to signal to emacs that we're done here.
	    (error "Refreshing %s in quarto-preview" name))))))))

(defun quarto-mode-markdown-command (begin-region end-region buf name)
  "Call quarto, typically from inside `markdown`.

Ensure quarto has rendered NAME (necessary if in a project).  If not in a project, render the region given by BEGIN-REGION and END-REGION, and insert the output of `quarto render` in BUF."

  ;; This handles previews (it all goes through quarto-preview instead of render)
  (quarto-mode--maybe-preview name)

  ;; This handles non-project files.
  ;; Quarto expects files to be in specific locations, so we
  ;; use make-temp-name instead of make-temp-file
  (let* ((file-directory (file-name-directory name))
	 (quarto-input-name (concat (make-temp-name file-directory) ".qmd"))
	 (quarto-output-name (concat (file-name-sans-extension quarto-input-name) ".html")))
    (write-region begin-region end-region quarto-input-name)
    (call-process quarto-command
		  nil
		  "*quarto-render-output*"
		  t
		  "render" quarto-input-name (concat "--output=" quarto-output-name) "--self-contained")
    (with-current-buffer buf
      (insert-file-contents quarto-output-name))
    (delete-file quarto-input-name)
    (delete-file quarto-output-name)))

(defvar quarto-mode-advice-installed nil)

(defun quarto-mode-default-hook ()
  "Set up the default file-local variables in quarto-mode."

  ;; This needs to happen in a hook rather than in mode-variables
  ;; because poly-mode uses other modes and we want to change their
  ;; own values

  ;; tell markdown-mode to use quarto-command for C-c C-c
  (setq markdown-command 'quarto-mode-markdown-command)
  (setq markdown-command-needs-filename t)
  (unless quarto-mode-advice-installed
    (advice-add #'fill-paragraph :around #'quarto-mode--fill-paragraph)
    (setq quarto-mode-advice-installed t)))

(defun quarto-mode-unload-function ()
  "Pre-cleanup when `unload-feature` is called."
  (when quarto-mode-advice-installed
    (advice-remove #'fill-paragraph #'quarto-mode--fill-paragraph)
    (setq quarto-mode-advice-installed nil)))

(add-hook 'poly-quarto-mode-hook #'quarto-mode-default-hook)

;;; Advice functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quarto-mode--is-block-p ()
  "Return non-nil if thing at point is a quarto block."
  (let ((this-para (thing-at-point 'paragraph t)))
    (or (quarto-mode--is-start-of-block-p this-para)
	(quarto-mode--is-end-of-block-p this-para))))

(defun quarto-mode--is-start-of-block-p (para)
  "Return non-nil if PARA is the start of a pandoc block."
  (string-match "^::: {[^}]+}" para))

(defun quarto-mode--is-end-of-block-p (para)
  "Return non-nil if PARA is the end of a pandoc block."
  (string-match ":::$" para))

(defun quarto-mode--fill-paragraph (orig-fun &rest args)
  "Fill paragraph in quarto mode.
Overrides `fill-paragraph` which is ORIG-FUN when necessary and
passes ARGS to it."
  (cond
   ((and (boundp 'poly-quarto-mode)
	 (quarto-mode--is-block-p))
    (let* ((para (thing-at-point 'paragraph t))
	   (is-start (quarto-mode--is-start-of-block-p para))
	   (is-end (quarto-mode--is-end-of-block-p para)))
      (save-excursion
	(when is-start
	  (re-search-backward "::: *{[^}]+}")
	  (re-search-forward "::: *{[^}]+}")
	  (insert "\n"))
	(when is-end
	  (re-search-forward ":::")
	  (re-search-backward ":::")
	  (insert "\n")
	  (forward-paragraph -2))
	(apply orig-fun args)
	(when is-start
	  (delete-char 1))
	(forward-paragraph)
	(when is-end
	  (delete-char 1)))))
   (t (apply orig-fun args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'quarto-mode)

;;; quarto-mode.el ends here

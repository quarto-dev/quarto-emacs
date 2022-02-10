;;; quarto-mode.el --- A (poly)mode for https://quarto.org -*- lexical-binding: t -*-
;;
;; Author: Carlos Scheidegger
;; Maintainer: Carlos Scheidegger
;; Copyright (C) 2022 RStudio PBC
;; Version: 0.0.1
;; package-requires: ((emacs "25") (polymode "0.2.2") (poly-markdown "0.2.2"))
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
(require 'poly-markdown)
(require 'shell)

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

;; this package doesn't require poly-R, ess-mode, or ess-r-mode, but
;; works differently when these are available.
(if (and (require 'ess-mode nil 'noerror)
	 (require 'ess-r-mode nil 'noerror)
	 (require 'poly-R nil 'noerror))
;;;###autoload (autoload 'poly-quarto-mode "quarto-mode")
    (define-polymode poly-quarto-mode poly-markdown+r-mode :lighter " Quarto")
;;;###autoload (autoload 'poly-quarto-mode "quarto-mode")
  (define-polymode poly-quarto-mode poly-markdown-mode :lighter " Quarto"))

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
  This, output file names don't comply with `polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(defcustom quarto-preview-display-buffer t
  "When nil, `quarto-preview' does not automatically display buffer."
  :group 'quarto
  :type 'boolean)

(polymode-register-exporter quarto-poly-markdown-exporter
			    nil poly-quarto-polymode)

(defun quarto-preview ()
  "Start a quarto preview process to automatically rerender documents.

  When run from a `_quarto.yml` buffer, this previews the entire book or
  website.  When run from a `.qmd` buffer, this previews that specific file.

  To control whether or not to show the display, customize
  `quarto-preview-display-buffer`."
  (interactive)
  (let* ((args
	  (cond
	   ((string-equal (file-name-nondirectory buffer-file-name) "_quarto.yml")
	    (list (format "quarto-preview-%s" buffer-file-name)
		  "*quarto-preview*"
		  "quarto"
		  "preview"))
	   (t
	    (list (format "quarto-preview-%s" buffer-file-name)
		  "*quarto-preview*"
		  "quarto"
		  "preview"
		  buffer-file-name))))
	 (process (apply #'start-process args)))
    (with-current-buffer (process-buffer process)
      (when quarto-preview-display-buffer
	(display-buffer (current-buffer)))
      (shell-mode)
      (set-process-filter process 'comint-output-filter))))

(easy-menu-define quarto-menu
  (list poly-quarto-mode-map)
  "Menu for quarto-mode"
  '("Quarto"
    ["Start Preview" quarto-preview t]))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))


(provide 'quarto-mode)

;;; quarto-mode.el ends here

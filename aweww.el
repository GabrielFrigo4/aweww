;;; aweww.el --- Awesome EWW

;; TODO
;; [ ] Improve Dark / Light Themes
;; [X] Improve HTML Rendering Indentation
;; [X] Improve Code Highlight Scope

;; ################
;; # EWW / SHR
;; ################


;; Require EWW / SHR / DOM
(require 'eww)
(require 'shr)

;; Require Packages
(require 'shr-tag-pre-highlight)

;; Enable EWW Readable
(add-hook 'eww-after-render-hook #'eww-readable)


;; ################
;; # Renders
;; ################


;; Import SHRFACE
(require 'shrface)

;; Cleanup New Lines
(defun aweww-cleanup-newlines ()
  "Remove Excessive Blank Lines in AWEWW Buffers."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n")))))

;; Cleanup New Lines Deferred
(defun aweww-cleanup-newlines-deferred ()
  "Defer cleanup until after EWW finishes rendering."
  (run-at-time 0 nil #'aweww-cleanup-newlines))

;; Setup Shrface in EWW
(defun shrface-eww-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp)))

;; AWEWW General Rendering
(defvar aweww-general-rendering-functions
  (append '((title . eww-tag-title)
            (form . eww-tag-form)
            (input . eww-tag-input)
            (button . eww-form-submit)
            (textarea . eww-tag-textarea)
            (select . eww-tag-select)
            (link . eww-tag-link)
            (meta . eww-tag-meta)
            (code . shrface-tag-code)
            (pre . shr-tag-pre-highlight))
          shrface-supported-faces-alist)
  "Aweww General Rendering Functions")

;; Enable Shrface in AWEWW
(setq-default shr-external-rendering-functions aweww-general-rendering-functions)

;; AWEWW Render Advice
;;(defun aweww-render-advice (orig-fun &rest args)
;;  (let ((shrface-org nil)
;;        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
;;        (shr-width 128)
;;        (shr-indentation 0)
;;        (shr-external-rendering-functions aweww-general-rendering-functions)
;;        (shr-use-fonts nil))
;;    (apply orig-fun args)))

;; Updae EWW Render
(add-hook 'eww-after-render-hook #'shrface-eww-setup)
(add-hook 'eww-after-render-hook #'aweww-cleanup-newlines-deferred)

;; ################
;; # AWEWW
;; ################


;; Alias to AWEWW
(defalias 'aweww 'eww)

;; Provide *aweww*
(provide 'aweww)

;;; aweww.el ends here
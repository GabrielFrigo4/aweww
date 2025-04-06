;;; aweww.el --- Awesome EWW

;; ################
;; # EWW / SHR
;; ################

;; Require EWW / SHR
(require 'eww)
(require 'shr)

;; Cleanup New Lines
(defun aweww-cleanup-newlines ()
  "Remove Excessive Blank Lines in AWEWW Buffers."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n")))))
(add-hook 'eww-after-render-hook #'aweww-cleanup-newlines)

;; ################
;; # Renders
;; ################

;; Require DOM
(require 'dom)

;; Create *EWW After Section Header*
(defvar aweww-after-section-header nil
  "AWEWW After Section Header")

;; Restart *EWW After Section Header*
(add-hook 'eww-after-render-hook
          (lambda ()
            (setq aweww-after-section-header nil)))

;; Create <b> Render for EWW / SHR
(defun aweww-render-b (dom)
  "Custom <b> rendering for AWEWW: Headers and Body"
  (let* ((class (dom-attr dom 'class))
         (text (dom-texts dom)))
    (cond
     ;; Section Header: Insert Spacing and New Line
     ((and class (string= class "section"))
      (shr-insert "\n\n")
      (shr-generic dom)
      (shr-insert "\n\n")
      (setq aweww-after-section-header t))
     ;; Bold Text: Indent if at Beginning of Line
     ((and text
           (or (string-match-p "^[a-zA-Z0-9_]+$" text)
               (string-match-p "^#" text)))
      (when (and (save-excursion (bolp))
                 (eq aweww-after-section-header t))
        (shr-insert "     ")
        (setq aweww-after-section-header nil))
      (shr-insert (propertize text 'face 'bold)))
     ;; Fallback
     (t
      (shr-generic dom)))))
(add-to-list 'shr-external-rendering-functions '(b . aweww-render-b))

;; ################
;; # AWEWW
;; ################

;; Alias to AWEWW
(defalias 'aweww 'eww)

;; Provide *aweww*
(provide 'aweww)

;;; aweww.el ends here
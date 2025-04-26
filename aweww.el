;;; aweww.el --- Awesome EWW


;; ################
;; # EWW / SHR
;; ################


;; Require EWW / SHR / DOM
(require 'eww)
(require 'shr)
(require 'dom)

;; Require Packages
(require 'shr-tag-pre-highlight)


;; ################
;; # Renders
;; ################


;; Aweww Org Code
(defvar aweww-org-code-fontify nil
  "Aweww Org Code Fontify")

;; Aweww Trim Code
(defun aweww-trim-code (string)
  "Remove Leading and Trailing Lines that Contain only Whitespace, Preserving Internal Formatting."
  (let* ((lines (split-string string "\n"))
         (lines (seq-drop-while (lambda (line) (string-match-p "\\`\\s-*\\'" line)) lines))
         (lines (seq-reverse
                 (seq-drop-while (lambda (line) (string-match-p "\\`\\s-*\\'" line))
                                 (seq-reverse lines)))))
    (mapconcat 'identity lines "\n")))

;; Aweww General Rendering
(defvar aweww-general-rendering-functions
  '((title . eww-tag-title)
    (form . eww-tag-form)
    (input . eww-tag-input)
    (button . eww-form-submit)
    (textarea . eww-tag-textarea)
    (select . eww-tag-select)
    (link . eww-tag-link)
    (meta . eww-tag-meta)
    (code . shrface-tag-code)
    (pre . aweww-shr-tag-pre-highlight))
  "Aweww General Rendering Functions")

;; Aweww Render Advice
(defun aweww-render-advice (orig-fun &rest args)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-width 128)
        (shr-indentation 0)
        (shr-external-rendering-functions aweww-general-rendering-functions)
        (shr-use-fonts nil))
    (apply orig-fun args)))

;; Aweww TAG Highlight
(defun aweww-shr-tag-pre-highlight (pre)
  "Highlighting Code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 (indent-rigidly (point-min) (point-max) 0)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (if aweww-org-code-fontify
         (progn
           (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
           (or (and (fboundp mode)
                    (with-demoted-errors "Error while fontifying: %S"
                      (shr-tag-pre-highlight-fontify (propertize (aweww-trim-code code) 'face 'org-block) mode)))
               (propertize (aweww-trim-code code) 'face 'org-block))
           (propertize (concat "#+BEGIN_SRC" "\n") 'face 'org-block-end-line))))
    (shr-ensure-newline)
    (setq end (point))
    (pcase (frame-parameter nil 'background-mode)
      ('light
       (add-face-text-property start end '(:background (face-background 'default nil t) :extend t)))
      ('dark
       (add-face-text-property start end '(:background (face-background 'default nil t) :extend t))))
    (shr-ensure-newline)
    (insert "\n")))

;; Updae EWW Render
(advice-add 'eww-display-html :around #'aweww-render-advice)


;; ################
;; # AWEWW
;; ################


;; Alias to AWEWW
(defalias 'aweww 'eww)

;; Provide *aweww*
(provide 'aweww)

;;; aweww.el ends here
;;; sleet-mode.el -- Major mode for editing Sleet files

;;; Commentary:

;;; Code:

(defun sleet-find-indent-token ()
  "Find The first."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\n\\([ \t]+\\)[^ \t]+")
      (match-string 1))))

(defun sleet-font-lock-region-backward-set-block-region ()
  "Set font-lock-beg and font-lock-end to current block."
  (beginning-of-line)
  (when (re-search-forward "\\([ \t]+\\)[^ \t]+[^\n]*\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" nil t)
    (setq font-lock-beg (match-beginning 0))
    (setq font-lock-end (1- (match-end 0))) t))

(defun sleet-font-lock-region-backward (matcher)
  "Find the nearest parent line, and call the matcher.
MATCHER is used to test if the current if a block starter."

  (beginning-of-line)
  (when (re-search-forward "^\\([ \t]+\\)[^ \t]+" (line-end-position) t)
    (let ((tl (1- (length (match-string 1)))))
      (when (re-search-backward (concat "^\\([ \t]\\)\\{," (number-to-string tl) "\\}[^ \t\n]+") nil t)
        (beginning-of-line)
        (if (funcall matcher)
            (sleet-font-lock-region-backward-set-block-region)
          (sleet-font-lock-region-backward matcher))))))

(defun sleet-font-lock-region-forward ()
  "Look forward to enlarge font lock region."
  (goto-char font-lock-beg)
  (beginning-of-line)
  (let ((pp (point)))
    (when (re-search-forward "\\([ \t]+\\)[^ \t]+[^\n]*\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" font-lock-end t)
      (setq font-lock-beg (min pp (match-beginning 0)))
      (setq font-lock-end (max font-lock-end (1- (match-end 0)))) t)))

(defun sleet-font-lock-region-start-from-beginning ()
  "Each region should start from beginning of line."
  (goto-char font-lock-beg)
  (let ((pp (point)))
    (beginning-of-line)
    (when (/= pp (point))
      (setq font-lock-beg (min pp (point))) t)))

(defun sleet-extend-font-lock-region ()
  "Function that used to unsure font lock region."
  (save-excursion
    (or
     (sleet-font-lock-region-backward (lambda () (re-search-forward "^[ \t]*[^\n]+\\.[ \t]*$" (line-end-position) t)))
     (sleet-font-lock-region-forward)
     (sleet-font-lock-region-start-from-beginning)
     )))

(defun sleet-highlight-comment-block (limit)
  "Highlight comment block.  LIMIT is feeded by font-lock."
  (when (re-search-forward "\\([ \t]*\\)#\\.[ \t]*\n\\(\\1[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+" limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-comment-face)
    t))

(defconst sleet-font-lock-keywords
  `(("doctype" . font-lock-comment-face)
    ("^#![^\n]*$" . font-lock-comment-face)
    ("^[ \t]*\\([a-zA-Z$@_0-9\-]+\\)" 1 font-lock-type-face)
    ("\\([<>+:]\\)[ \t]+\\([a-zA-Z$@_0-9\-]+\\)" . (
                                                (1 font-lock-keyword-face t)
                                                (2 font-lock-type-face t)
                                                ))
    ("^[ \t]*\\([#.][a-zA-Z$@_0-9\-]+\\)" . font-lock-constant-face)
    sleet-highlight-comment-block
    ))

(defvar sleet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# ". 1" table)
    (modify-syntax-entry ?\  ". 2" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for sleet-mode.")

(define-derived-mode sleet-mode fundamental-mode "Sleet"
  "Major mode for editing sleet files"

  (set-syntax-table sleet-mode-syntax-table)

  (set (make-local-variable 'comment-start) "\\(# \\)")
  (set (make-local-variable 'comment-end) "")

  (setq font-lock-extend-region-functions '(sleet-extend-font-lock-region))
  (set (make-local-variable 'jit-lock-contextually) t)
  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(sleet-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.sleet$" . sleet-mode))

(provide 'sleet-mode)

;;; sleet-mode ends here

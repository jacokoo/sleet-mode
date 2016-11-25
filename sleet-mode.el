;;; sleet-mode.el -- Major mode for editing Sleet files

;;; Commentary:

;;; Code:

(defgroup sleet nil
  "A Sleet major mode."
  :group 'languages)

(defcustom sleet-indent-token "    "
  "Default indent token")

(defun sleet-find-indent-token ()
  "Find The first."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (or
     (when (re-search-forward "\n\\([ \t]+\\)[^ \t]+" nil t) (match-string 1))
     sleet-indent-token)))

(defun sleet-indent-line ()
  ())


(defun sleet-comment-dwim (arg)
  (interactive "*P")
  (if (use-region-p)
      (sleet-comment-uncomment-region)
    (sleet-comment-uncomment-line)))

(defun sleet-comment-uncomment-line ()
  (save-excursion
    (goto-char (point-at-bol))
    (if (looking-at "^[ \t]*# ")
        (uncomment-region (point-at-bol) (point-at-eol))
      (comment-region (point-at-bol) (point-at-eol)))))

(defun sleet-comment-uncomment-region ()
  (save-excursion
    (let ((beg (region-beginning)) (end (region-end)))
      (goto-char beg)
      (setq beg (point-at-bol))

      (goto-char end)
      (if (eq end (point-at-bol)) (backward-char) (goto-char (point-at-eol)))
      (setq end (point))

      (if (sleet-all-line-commented-p beg end)
          (uncomment-region beg end)

        (goto-char beg)
        (while (< (point) end)
          (when (not (looking-at "^[ \t]*# ")) (comment-or-uncomment-region (point) (point-at-eol)))
          (forward-line))))))

(defun sleet-all-line-commented-p (beg end)
  (let ((result t))
    (goto-char beg)
    (while (and result (< (point) end))
      (if (looking-at "\\([ \t]*# \\|^[ \t]*\n\\)")
          (forward-line)
        (setq result nil)))

    result))

(defconst sleet-re-identifier "[a-zA-Z$@_][a-zA-Z0-9$_\-]*")
(defconst sleet-re-tag-name (concat "\\(" sleet-re-identifier ":\\)?\\(" sleet-re-identifier "\\|#\\(?: \\|\\.\\)\\||\\)"))
(defconst sleet-re-class-selector (concat "\\(\\." sleet-re-identifier "\\)"))
(defconst sleet-re-id-selector (concat "\\(#" sleet-re-identifier "\\)"))
(defconst sleet-re-inline-indicator "\\([><:+][ \t]+\\)")
(defconst sleet-re-attribute-key (concat
                                  ;; namespace
                                  "\\(" sleet-re-identifier ":\\)?"

                                  ;; key content
                                  "\\([^ \t=)]+\\)"))
(defconst sleet-re-attribute-value (concat
                                    ;; + prefix
                                    "[ \t]*\\(+[ \t]*\\)?\\(?:"

                                    ;; quoted string
                                    "\\([\"'][^\"']+[\"']\\)\\|"

                                    ;; numbers
                                    "\\([0-9]+\\)\\|"

                                    ;; boolean
                                    "\\(true\\|false\\)\\|"

                                    ;; helper
                                    "\\(" sleet-re-identifier "\\)(\\|"

                                    ;; identifier
                                    "\\([^ \t\n)+=]+\\)\\)"
                                    ))

(defconst sleet-re-whole-line (concat
                               ;; prefix
                               "\\(^[ \t]*\\|" sleet-re-inline-indicator  "\\)"

                               ;; tag name or class selector or id selector
                               "\\(" sleet-re-tag-name "\\)?\\(" sleet-re-class-selector "\\|" sleet-re-id-selector "\\)*"

                               ;; attribute start
                               "[ \t]*\\(([ \t]*\\("

                               ;; attribute key
                               "\\(" sleet-re-attribute-key "[ \t]*=\\)?"

                               ;; attribute value
                               "\\(" sleet-re-attribute-value "\\)+"

                               ;; attribute end
                               "\\)+[ \t]*)\\)?[ \t]*\n*"
                               ))

(defun sleet-highlight-whole-line (limit)
  "Highlight line.
LIMIT the end position to do search."

  ;; start with tag name or class selector or id selector
  ;; (message "whole line (%s, %s)" (point) limit)
  (while (and (> limit (point)) (re-search-forward (concat
                            "^[ \t]*\\(?:"
                            sleet-re-tag-name "\\|"
                            sleet-re-class-selector "\\|"
                            sleet-re-id-selector "\\)")
                            limit t))
    (when (match-string 1) (put-text-property (match-beginning 1) (match-end 1) 'face font-lock-function-name-face))
    (when (match-string 2)
      (let ((str (match-string 2)) (sb (match-beginning 2)) (se (match-end 2)))
        (cond
         ((string= "# " str) (put-text-property sb se 'face font-lock-comment-face))
         ((string= "#." str) (put-text-property sb se 'face font-lock-comment-face) (backward-char))
         ((string= "|" (match-string 2)) (put-text-property sb se 'face font-lock-string-face))
         (t (put-text-property sb se 'face font-lock-type-face)))))
    (when (match-string 3) (put-text-property (match-beginning 3) (match-end 3) 'face font-lock-constant-face))
    (when (match-string 4) (put-text-property (match-beginning 4) (match-end 4) 'face font-lock-constant-face))
    (let ((tag-name (match-string 2)))
      (sleet-highlight-following-selectors (line-end-position))
      (sleet-highlight-attribute-groups limit)
      (sleet-highlight-inline-tags limit)
      (sleet-highlight-text-block tag-name)
      )))

(defun sleet-highlight-inline-tags (limit)
  "Highlight inline tags.
LIMIT the end position to do search."
  (while (looking-at (concat
                      "[ \t]*"
                      sleet-re-inline-indicator
                      "\\(?:"
                      sleet-re-tag-name "\\|"
                      sleet-re-class-selector "\\|"
                      sleet-re-id-selector "\\)"))

    (when (match-string 1) (put-text-property (match-beginning 1) (match-end 1) 'face font-lock-keyword-face))
    (when (match-string 2) (put-text-property (match-beginning 2) (match-end 2) 'face font-lock-function-name-face))
    (when (match-string 3) (put-text-property (match-beginning 3) (match-end 3) 'face font-lock-type-face))
    (when (match-string 4) (put-text-property (match-beginning 4) (match-end 4) 'face font-lock-constant-face))
    (when (match-string 5) (put-text-property (match-beginning 5) (match-end 5) 'face font-lock-constant-face))
    (goto-char (match-end 0))
    (let ((tag-name (match-string 3)))
      (sleet-highlight-following-selectors (line-end-position))
      (sleet-highlight-attribute-groups limit)
      (sleet-highlight-text-block tag-name))))

(defun sleet-highlight-following-selectors (limit)
  "Highlight following class selector or id selector
LIMIT is the end position to do search"
  (while (looking-at (concat "\\(?:" sleet-re-class-selector "\\|" sleet-re-id-selector "\\)"))
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-constant-face)
    (goto-char (match-end 0))
    ))

(defun sleet-highlight-attribute-groups (limit)
  "Highlight attribute groups
LIMIT is the end position to do search"
  (while (looking-at "[ \t]*(")
    (goto-char (match-end 0))
    (sleet-highlight-attribute-pair 0)
    (when (re-search-forward ")" limit t)

      ;; highlight group helper
      (when (looking-at (concat "[ \t]*&[ \t]*" sleet-re-identifier))
        (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-function-name-face)
        (goto-char (match-end 0))
        (sleet-highlight-attribute-groups limit)))))

(defun sleet-highlight-attribute-pair (helper-count)
  "Highlight attribute pairs.
HELPER-COUNT is used to match helper."
  (let ((hc helper-count))
    (if (looking-at (concat
                     "[ \t\n]*\\(?:"
                     sleet-re-attribute-key
                     "[ \t]*\\(=\\)\\)?"
                     sleet-re-attribute-value
                     ))
        (progn

          ;; attribute key namespace
          (when (match-string 1) (put-text-property (match-beginning 1) (match-end 1) 'face font-lock-function-name-face))

          ;; attribute key
          (when (match-string 2) (put-text-property (match-beginning 2) (match-end 2) 'face font-lock-variable-name-face))

          ;; =
          (when (match-string 3) (put-text-property (match-beginning 3) (match-end 3) 'face font-lock-constant-face))

          ;; +
          (when (match-string 4) (put-text-property (match-beginning 4) (match-end 4) 'face font-lock-constant-face))

          ;; quoted string
          (when (match-string 5) (put-text-property (match-beginning 5) (match-end 5) 'face font-lock-string-face))

          ;; numbers
          (when (match-string 6) (put-text-property (match-beginning 6) (match-end 6) 'face font-lock-keyword-face))

          ;; true false
          (when (match-string 7) (put-text-property (match-beginning 7) (match-end 7) 'face font-lock-keyword-face))

          ;; helper, when helper is found, extend the end of attribute group to next ")"
          (when (match-string 8)
            (put-text-property (match-beginning 8) (match-end 8) 'face font-lock-function-name-face)
            (setq hc (1+ hc)))

          ;; identifier
          (when (match-string 9) (put-text-property (match-beginning 9) (match-end 9) 'face font-lock-keyword-face))
          (goto-char (match-end 0))
          (sleet-highlight-attribute-pair hc))

      (when (and (> hc 0) (looking-at "[ \t]*)"))
        (goto-char (match-end 0))
        (sleet-highlight-attribute-pair (1- hc))))))

(defun sleet-highlight-text-block (tag-name)
  "Skip text block."
  (when (looking-at "[ \t]*\\.[ \t]*$")
    (let ((face-name (cond
                      ((string= "#." tag-name) font-lock-comment-face)
                      ((string= "|" tag-name) font-lock-string-face)
                      (t font-lock-string-face))))
      (beginning-of-line)
      (looking-at "[ \t]*")
      (forward-line 1)
      (when (looking-at (concat "\\(^" (match-string 0) "[ \t]+[^\n]*\n\\|^[ \t]*\n\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'face face-name)
        (goto-char (match-end 0))))))

(defun sleet-re-gen-block (re)
  "return a regexp to match sleet block.
RE is used to match block start to identify what kind of block will be matched."
  (concat
   ;; block indent
   "^\\([ \t]*\\)"
   re
   ;; block always ends up with a trailing DOT
   "\\.[ \t]*\n"
   ;; the following lines which with deeper indent is the content of the block
   "\\(\\1[ \t]+[^\n]*\n?"
   ;; skip empty lines
   "\\|^[ \t]*\n\\)+"
   ))

(defconst sleet-re-any-block (sleet-re-gen-block "[^ \t\n]+")
  "Any line that end with a DOT.")

(defconst sleet-re-comment-block (sleet-re-gen-block "#")
  "Comment block start by #.")

(defconst sleet-re-text-block (sleet-re-gen-block "|")
  "Text block start by |.")

(defun sleet-highlight-comment-block (limit)
  "Highlight comment block.
LIMIT is feeded by font-lock."
  (when (re-search-forward sleet-re-comment-block limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'face font-lock-comment-face)
    t))

(defun sleet-font-lock-extend-end-of-region ()
  "This function will be called two way:
1. Once file be opened, Font Lock will seperate it in regions from top to
bottom, and then call this function to ensure the region boundaries.
2. After file be changed, Font Lock will call after-change-hood to ensure the
region boundaries and then call this function to ensure again.
So this function only have to ensure the END of the region."

  (message "extend region: (%s, %s)" font-lock-beg font-lock-end)
  (save-excursion
    ;; move point to the start of the next line
    ;; to ensure the last line will be searched
    (goto-char font-lock-end)
    (message "search start at %s" (point))

    ;; it may have two or more blocks in a single region
    ;; search backward to get the last one
    (when (re-search-backward sleet-re-any-block font-lock-beg t)
      ;; when got a block, test if the end of block is nested in current region
      (let ((end (match-end 0)) (token (match-string 1)))
        (when (and
               (eq end font-lock-end)

               ;; token is the indent of current block
               ;; any following lines have deeper indent are the content of current block
               (re-search-forward (concat "\\(^" token "[ \t]+[^\n]*\n?\\|^[ \t]*\n\\)+") nil t))

          (message "set region end to %s" (match-end 0))
          ;; set font-lock-end to match end
          ;; backward one character to keep the point at the end of current block
          (setq font-lock-end (1- (match-end 0))) t)))))

(defun sleet-font-lock-find-parent (beg)
  "Recursive find parent to determine if current line is content of block.
BEG is the point to start search.
return the (start end) of the found block otherwise return nil"

  (goto-char beg) (beginning-of-line)

  ;; test if current line ends up with a DOT
  (if (re-search-forward "^\\([ \t]*\\)[^\n]+\\.[ \t]*$" (line-end-position) t)

      ;; find the block end point
      (progn
        (let ((start (line-beginning-position)))
          (when (re-search-forward (concat "\\(^" (match-string 1) "[ \t]+[^\n]*\n\\|^[ \t]*\n\\)+") nil t)
            (cons start (match-end 0)))))

    ;; find the parent of current line
    ;; get the indent of current line
    (when (re-search-forward "^\\([ \t]+\\)[^ \t]+" (line-end-position) t)
      (let ((token-length (1- (length (match-string 1)))))

        ;; backward find the line have less indent
        (when (re-search-backward (concat "^\\([ \t]\\)\\{," (number-to-string token-length) "\\}[^ \t\n]+") nil t)
          (sleet-font-lock-find-parent (match-beginning 0)))))))

(defun sleet-font-lock-extend-beginning-of-region (beg end old-len)
  "This function is used as after-change-hook.

BEG END OLD-LEN are all feeded by font-lock.
This will return a cons present the matched region."
  (message "after change %s %s" beg end)
  (save-excursion
    (let ((result (sleet-font-lock-find-parent beg)))
      (when result
        (message "set region beginning to %s" result)

        (put-text-property (car result) (cdr result) 'font-lock-multiline t)
        ;; use orignial end
        ;; result
        nil
        ))))

(defconst sleet-font-lock-keywords
  `(("doctype" . font-lock-comment-face)
    ("^#![^\n]*$" . font-lock-comment-face)
    ;; ("^[ \t]*\\([a-zA-Z$@_0-9\-]+\\)" 1 font-lock-type-face)
    ;; ("\\([<>+:]\\)[ \t]+\\([a-zA-Z$@_0-9\-]+\\)" . (
    ;;                                             (1 font-lock-keyword-face t)
    ;;                                             (2 font-lock-type-face t)
    ;;                                             ))
    ;; ("^[ \t]*\\([#.][a-zA-Z$@_0-9\-]+\\)" . font-lock-constant-face)
    ;; (sleet-re-whole-line . font-lock-constant-face)
    sleet-highlight-whole-line
    ;; sleet-highlight-comment-block
    ))

(defvar sleet-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\' "w" table)
    table)
  "Syntax table for sleet-mode.")

(define-derived-mode sleet-mode fundamental-mode "Sleet"
  "Major mode for editing sleet files"

  (set-syntax-table sleet-mode-syntax-table)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "# [ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(sleet-font-lock-keywords))
  ;; (setq font-lock-extend-region-functions '(sleet-font-lock-extend-end-of-region))
  ;; (make-local-variable 'font-lock-extend-region-functions)
  ;; (add-to-list 'font-lock-extend-region-functions 'sleet-font-lock-extend-end-of-region t)
  (set (make-local-variable 'font-lock-extend-after-change-region-function) 'sleet-font-lock-extend-beginning-of-region)

  (define-key sleet-mode-map [remap comment-dwim] 'sleet-comment-dwim)

  )

(add-to-list 'auto-mode-alist '("\\.sleet$" . sleet-mode))

(provide 'sleet-mode)

;;; sleet-mode ends here

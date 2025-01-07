;;; simpc-mode.el --- A simple C mode

;; Copyright (C) 2020 Alexey Kutepov <reximkut@gmail.com>

;; Author: Alexey Kutepov <reximkut@gmail.com>
;;         Dave Voutila <voutilad@gmail.com>
;; Maintainer: Dave Voutila <voutilad@gmail.com>

;;; Code:
(require 'subr-x)

(defvar simpc-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff?
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun simpc-types ()
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"
    "paddr_t" "vaddr_t"
    "off_t" "uid_t" "pid_t"
    "pt_entry_t"))

(defun simpc-keywords ()
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return"  "sizeof" "static" "struct" "switch" "typedef"
    "union"  "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch"  "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false"
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun simpc-font-lock-keywords ()
  (list
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (simpc-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (simpc-types) 'symbols) . font-lock-type-face)))

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun simpc--previous-non-empty-line-2 ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (simpc--previous-non-empty-line)))

(defun simpc--indentation-of-previous-ifelse ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (not (string-match-p
                      (rx (0+ space) "if"
                          (0+ space) "("
                          (+? anything) line-end)
                      (string-trim-right
                       (thing-at-point 'line t)))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--indentation-of-open-brace ()
  "Find the indentation level at the most likely matching open brace."
  (save-excursion
    (forward-line -1)
    (let ((x 0))
      (while (and (>= x 0)
                  (not (bobp)))
        (let* ((line (thing-at-point 'line t))
               (openings (+ (length (split-string line "{")) -1))
               (closings (+ (length (split-string line "}")) -1)))
          (setq x (+ x (- closings openings)))
          (forward-line -1)))
      (current-indentation))))

(defun simpc--indentation-of-open-comment ()
  "Find the indentation level at the most recent opening C-style comment."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (not (string-suffix-p "/*" (string-trim-right
                                            (thing-at-point 'line t)))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (cur-indent (current-indentation))
         (now (current-time-string))
         (prev-line (string-trim-right (simpc--previous-non-empty-line)))
         (prev-line-2 (string-trim-right (simpc--previous-non-empty-line-2)))
         (prev-indent-ifelse (simpc--indentation-of-previous-ifelse))
         (prev-indent (simpc--indentation-of-previous-non-empty-line))
         (indent-len 8))

    ;; NOTE: if cur-line == prev-line, user just hit return

    (message
     "\ncl: %s\npl: %s\npl2: %s\npi-ifelse: %d\ncur-indent: %d, prev: %d"
     cur-line prev-line prev-line-2 prev-indent-ifelse cur-indent prev-indent)
    (cond
     ;; switch opening
     ((string-match-p
       (rx (0+ space) "switch" (+? anything) line-end) prev-line)
      (progn (message "%s: switch open" now)
             prev-indent))

     ;; TODO: multi-line function call (4-space indent)

     ;; ((and (string-suffix-p "{" prev-line)
     ;;       (string-prefix-p "}" (string-trim-left cur-line)))
     ;;  prev-indent)

     ((string-suffix-p "/*" prev-line)
      (progn (message "%s: multi-line comment cont." now)
             (+ prev-indent 1)))

     ((string-prefix-p "* " (string-trim-left prev-line))
      (progn (message "%s: multi-line comment body" now)
             prev-indent))

     ((string-suffix-p "*/" prev-line)
      (let ((indent (simpc--indentation-of-open-comment)))
        (message "%s: multi-line comment end" now)
        (- prev-indent 1)))

     ;; Identify indent if previous line ends with an open-brace, accounting
     ;; for multi-line conditionals (if/else/if else).
     ;; ((string-suffix-p "{" prev-line)
     ;;  (progn (message "HEY YO YO")
     ;;         (if (eq 0 (% prev-indent indent-len))
     ;;             (+ prev-indent indent-len)
     ;;           (+ prev-indent 4))))

     ;; ((string= "{" prev-line)
     ;;  (progn (message "%s: after new brace" now) 8))

     ((string-suffix-p "{" prev-line)
      (progn (message "%s: after open brace" now)
             (+ prev-indent indent-len)))

     ;; Outdent a line with just a closing brace, using the indentation of
     ;; the likely matching opening brace.
     ((and (eq 0 (% cur-indent indent-len))
           (string-prefix-p "}" (string-trim-left cur-line)))
      (let ((x (simpc--indentation-of-open-brace)))
        (message "%s: solo closing brace" now)
        (* indent-len (/ x indent-len))))

     ;; Handle switch case's, outdenting or indenting as needed.
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          (progn (message "%s: fallthrough case" now)
                 prev-indent)
        (progn (message "%s: new case" now))
        (+ prev-indent indent-len)))

     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))

;;; function definition continuation

     ((string-suffix-p "," prev-line)
      (if (< 0 (length (string-trim cur-line)))
          (progn (message "%s: func def cont. start" now)
                 (+ prev-indent 4))
        (progn (message "%s: func def cont. more" now)
               prev-indent)))

     ;; Cheat to deal with stuff already far left aligned, like functions, etc.
     ;; ((and (<= (current-indent) indent-len)
     ;;       (string-suffix-p ")" prev-line))
     ;;  (progn (message "pooooooooP") 0))

     ((and (= 0 (current-indentation))
           (string-prefix-p "{" cur-line))
      (progn (message "%s: function body begin" now)
             indent-len))

     ;; 1-liner if/else-if's
     ((string-match-p
       (rx (0+ space) (or "if" "else if")
           (0+ space) "(" (+? anything) ")"
           (0+ space) line-end)
       prev-line)
      (progn (message "%s: one-shot if/else")
             (+ prev-indent indent-len)))

     ((string-match-p
       (rx (0+ space) "else"
           (0+ space) line-end)
       prev-line)
      (progn (message "%s: one-shot else" now)
             (+ prev-indent indent-len)))

     ((string-match-p
       (rx (0+ space) (or "if" "else if")
           (0+ space) "(" (+? anything) ")"
           (0+ space) line-end)
       prev-line-2)
      (progn (message "%s: outdent after one-shot if/else" now)
             prev-indent-ifelse))

     ((string-match-p
       (rx (0+ space) "else" (0+ space)) prev-line-2)
      (progn (message "%s: one-shot else" now)) prev-indent-ifelse)

     ;;; if/else-if continuations
     ;; Identify if we need to do a half-indent due to open if/else condition
     ((string-match-p
       (rx (0+ space) (or "if" "else if") (1+ space)
           "(" (+? anything) (0+ space) line-end)
       prev-line)
      (progn (message "%s: if/else cont." now) (+ prev-indent 4)))

     ((string-match-p
       (rx line-start (and (not space) (not "{"))) prev-line)
      (progn (message "%s: CHEAT" now) prev-indent))


     ;; xxxxx
     ((string-match-p
       (rx (1+ space) (+? anything) ")" (0+ space) (0+ "{")
           (0+ space) line-end)
       prev-line)
      (progn (message "%s: poop HERE" now)
             (+ prev-indent-ifelse indent-len)))

     ((and (<= 8 prev-indent-ifelse)
           (string-match-p
            (rx (+? anything) ")" (0+ space) (0+ "{") (0+ space) line-end)
            prev-line-2))
      (progn (message "%s: not sure yet" now) prev-indent-ifelse))

     ((string-suffix-p ";" prev-line)
      (progn (message "%s: new statement" now) prev-indent))

     ;; Just use previously found indentation.
     (t (progn (message "%s: default" now) prev-indent)))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-tabs-mode nil)
      (message ">> indenting to %d" desired-indentation)
      (indent-line-to desired-indentation)
      (forward-char n))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "// "))

(provide 'simpc-mode)

;;; simpc-mode.el ends here

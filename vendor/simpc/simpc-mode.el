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
    (let ((x 0))
      (while (and (>= x 0)
                  (not (bobp)))
        (forward-line -1)
        (let* ((line (thing-at-point 'line t))
               (openings (+ (length (split-string line "{")) -1))
               (closings (+ (length (split-string line "}")) -1)))
          (setq x (+ x (- closings openings)))))
      (current-indentation))))

(defun simpc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (simpc--previous-non-empty-line)))
         (prev-line-2 (string-trim-right (simpc--previous-non-empty-line-2)))
         (prev-indent-ifelse (simpc--indentation-of-previous-ifelse))
         (prev-indent (simpc--indentation-of-previous-non-empty-line))
         (indent-len 8))

    (message "cl: %s\npl: %s\npl2: %s\npi-ifelse: %d"
             cur-line prev-line prev-line-2 prev-indent-ifelse)
    (cond
     ((string-match-p
       (rx (0+ space) "switch" (+? anything) line-end) prev-line)
      prev-indent)


     ;; ((and (string-suffix-p "{" prev-line)
     ;;       (string-prefix-p "}" (string-trim-left cur-line)))
     ;;  prev-indent)

     ;; Identify indent if previous line ends with an open-brace, accounting
     ;; for multi-line conditionals (if/else/if else).
     ((string-suffix-p "{" prev-line)
      (if (eq 0 (% prev-indent indent-len))
          (+ prev-indent indent-len)
        (+ prev-indent 4)))

     ;; Outdent a line with just a closing brace, using the indentation of
     ;; the likely matching opening brace.
     ((string-prefix-p "}" (string-trim-left cur-line))
      (simpc--indentation-of-open-brace))

     ;; Handle switch case's, outdenting or indenting as needed.
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))

     ;; 1-liner if/else-if's
     ((string-match-p
       (rx (0+ space) (or "if" "else if")
           (0+ space) "(" (+? anything) ")"
           (0+ space) line-end)
       prev-line)
      (+ prev-indent indent-len))

     ((string-match-p
       (rx (0+ space) "else"
           (0+ space) line-end)
       prev-line)
      (+ prev-indent indent-len))

     ((string-match-p
       (rx (0+ space) (or "if" "else if")
           (0+ space) "(" (+? anything) ")"
           (0+ space) line-end)
       prev-line-2)
      prev-indent-ifelse)

     ((string-match-p
       (rx (0+ space) "else" (0+ space)) prev-line-2)
      prev-indent-ifelse)

     ;; if/else-if continuations
     ((string-match-p
       (rx (0+ space) (or "if" "else if") (1+ space)
           "(" (+? anything) (0+ space) line-end)
       prev-line)
      (+ prev-indent 4))
     ((string-match-p
       (rx (1+ space) (+? anything) ")"
           (0+ space) (0+ "{")
           (0+ space) line-end)
       prev-line)
      (+ prev-indent-ifelse indent-len))
     ((and (<= 8 prev-indent-ifelse)
           (string-match-p
            (rx (+? anything) ")" (0+ space) (0+ "{") (0+ space) line-end)
            prev-line-2))
      prev-indent-ifelse)

     ;; Just use previously found indentation.
     (t prev-indent))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-tabs-mode nil)
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

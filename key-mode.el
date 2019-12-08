;;; key-mode.el --- A mode for KeY files

;; Copyright (C) 2019 Alexander Weigl <weigl@kit.edu>
;; URL: https://github.com/wadoon/key-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-key-mode")
;; (require 'key-mode)

;;; Commentary:
;; This file provides the support for the rules and problems of the
;; KeY Theorem Prover `https://key-project.org'.
;;
;;
;; It provides following features:
;; 
;; - syntax highlighting 
;; - auto completion of keywords and sorts, functions, and predicates defined in the ldts
;; - snippets

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'cl-lib)


(defvar key-mode-keymap nil
  "The mode map for key-mode.")

(defvar key-mode-text-mode-syntax-table nil
  "The syntax table of key-mode.")

(defvar key-mode-font-lock nil)

(setq key-mode--keywords
      (list "\\sorts" "\\generic" "\\proxy" "\\extends" "\\oneof" "\\abstract"
            "\\schemaVariables" "\\schemaVar" "\\modalOperator" "\\program"
            "\\formula" "\\term" "\\update" "\\variables" "\\variable" "\\skolemTerm"
            "\\skolemFormula" "\\termlabel" "\\modifies" "\\programVariables" "\\sameObserver"
            "\\varcond" "\\applyUpdateOnRigid" "\\dependingOn" "\\disjointModuloNull"
            "\\dropEffectlessElementaries" "\\dropEffectlessStores" "\\simplifyIfThenElseUpdate"
            "\\enumConstant" "\\freeLabelIn" "\\hasSort" "\\fieldType" "\\final" "\\elemSort" "\\hasLabel"
            "\\hasSubFormulas" "\\isArray" "\\isArrayLength" "\\isConstant" "\\isEnumType"
            "\\isInductVar" "\\isLocalVariable" "\\isObserver" "\\different" "\\metaDisjoint"
            "\\isThisReference" "\\differentFields" "\\isReference" "\\isReferenceArray"
            "\\isStaticField" "\\sub" "\\equalUnique" "\\new" "\\newLabel" "\\containsAssignment"
            "\\not" "\\notFreeIn" "\\same" "\\static" "\\staticMethodReference" "\\mayExpandMethod"
            "\\strict" "\\typeof" "\\instantiateGeneric" "\\forall" "\u2200" "\\exists"
            "\u2203" "\\subst" "\\if" "\\ifEx" "\\then" "\\else" "\\include" "\\includeLDTs" "\\classpath"
            "\\bootclasspath" "\\noDefaultClasses" "\\javaSource" "\\withOptions" "\\optionsDecl"
            "\\settings" "\\profile" "true" "false" "\\sameUpdateLevel" "\\inSequentState"
            "\\antecedentPolarity" "\\succedentPolarity" "\\closegoal" "\\heuristicsDecl"
            "\\noninteractive" "\\displayname" "\\helptext" "\\replacewith" "\\addrules"
            "\\addprogvars" "\\heuristics" "\\find" "\\add" "\\assumes" "\\trigger"
            "\\avoid" "\\predicates" "\\functions" "\\transformers" "\\unique" "\\rules"
            "\\axioms" "\\problem" "\\chooseContract" "\\proofObligation" "\\proof"
            "\\proofScript" "\\contracts" "\\invariants" "\\lemma" "\\inType"
            "\\isAbstractOrInterface" "\\containerType" "\\locset" "\\seq" "\\bigint"
            "\u227A" "\u220A" "\u2205" "\u222A" "\u2229" "\u2286" "\u2216"))

(defconst key-mode-toplevel-keywords
  (regexp-opt '("\\javaSource" "\\chooseContract" "\\proofObligation"
                "\\bootclasspath" "\\problem" "\\include"
                "\\includeldt" "\\sorts" "\\profile" "\\preferences"
                "\\predicates" "\\functions" "\\withoptions" "\\programVariables")
	      t)
  "Toplevel keywords.")

(defconst key-mode-taclet-keywords
  (regexp-opt '("\\find" "\\assumes" "\\modality" "\\replacewith"
            	"\\add" "\\heuristics" "\\endmodality" "\\varcond"
            	"\\not" "\\hasSort" "\\isThisReference" "\\staticMethodReference"
            	"\\displayname" "\\sameUpdateLevel" "schemaVar" "\\modalOperator"
            	"\\new" "\\typeof" "\\term" "\\update" "\\formula")
                  t) "Keywords within taclets")

(setq key-mode-font-lock
      (let ((keywords-re (regexp-opt key-mode--keywords t)))
        `((,keywords-re . font-lock-keyword-face)
          ;; TODO #.* scheme variables
	  ;; TODO group (toplevel, rule level)
	  ;;("FIXME" . show-paren-mismatch-face)
          ;;(,key-mode-toplevel-keywords . font-lock-function)
       )))

(setq key-mode-syntax-table
      (let ((table (make-syntax-table)))
	;; ' is a string delimiter
	;;(modify-syntax-entry ?' "\"" table)
	;; " is a string delimiter too
	(modify-syntax-entry ?\" "\"" table)

	(modify-syntax-entry ?\\ "_" table)
	
	;; / is punctuation, but // is a comment starter
	(modify-syntax-entry ?/ ". 12" table)
	;; \n is a comment ender
	(modify-syntax-entry ?\n ">" table)
	table))

;; Create the keymap for this mode.
(setq key-mode-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;; Taken and adatped from julia-mode
;;   https://github.com/JuliaEditorSupport/julia-emacs/blob/master/julia-mode.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom key-indent-offset 4
  "Number of spaces per indentation level."
  :type 'integer
  :group 'key-mode)

(defun key-mode--in-comment (&optional syntax-ppss)
  "Return non-nil if point is inside a comment using SYNTAX-PPSS.
Handles both single-line and multi-line comments."
  (nth 4 (or syntax-ppss (syntax-ppss))))

(defun key-mode--in-string (&optional syntax-ppss)
  "Return non-nil if point is inside a string using SYNTAX-PPSS.
Note this is Emacs' notion of what is highlighted as a string.
As a result, it is true inside \"foo\", `foo` and 'f'."
  (nth 3 (or syntax-ppss (syntax-ppss))))

(defun key-mode-in-brackets ()
  "Return non-nil if point is inside square brackets."
  (let ((start-pos (point))
        (open-count 0))
    ;; Count all the [ and ] characters on the current line.
    (save-excursion
      (beginning-of-line)

      (while (< (point) start-pos)
        ;; Don't count [ or ] inside strings, characters or comments.
        (unless (or (key-mode-in-string) (key-mode-in-comment))

          (when (looking-at (rx "["))
            (incf open-count))
          (when (looking-at (rx "]"))
            (decf open-count)))

        (forward-char 1)))

    ;; If we've opened more than we've closed, we're inside brackets.
    (plusp open-count)))

(defun key-mode-at-keyword (kw-list)
  "Return the word at point if it matches any keyword in KW-LIST.
KW-LIST is a list of strings.  The word at point is not considered
a keyword if used as a field name, X.word, or quoted, :word."
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (looking-at "("))           ; handle "function(" when on (
       (member (current-word t) kw-list)
       ;; 'end' is not a keyword when used for indexing, e.g. foo[end-2]
       (or (not (equal (current-word t) "end"))
           (not (key-mode-in-brackets)))
       (not (key-mode-in-comment))))


(defun key-mode-safe-backward-sexp ()
  "if backward-sexp gives an error, move back 1 char to move over the '('."
  (if (condition-case nil (backward-sexp) (error t))
      (ignore-errors (backward-char))))

(defun key-mode-following-import-export-using ()
  "If the current line follows an `export` or `import` keyword
with valid syntax, return the position of the keyword, otherwise
`nil`. Works by stepping backwards through comma-separated
symbol, gives up when this is not true."
  ;; Implementation accepts a single Module: right after the keyword, and saves
  ;; the module name for future use, but does not enforce that `export` has no
  ;; module name.
  (let ((done nil)                      ; find keyword or give up
        (module nil))                   ; found "Module:"
    (save-excursion
      (beginning-of-line)
      (while (and (not done) (< (point-min) (point)))
        (key-mode-safe-backward-sexp)
        (cond
         ((looking-at (rx (or "import" "export" "using")))
          (setf done (point)))
         ((looking-at (rx (group (* (or word (syntax symbol)))) (0+ space) ":"))
          (if module
              (setf done 'broken)
            (setf module (match-string-no-properties 1))))
         ((looking-at (rx (* (or word (syntax symbol))) (0+ space) ","))
          (when module (setf done 'broken)))
         (t (setf done 'broken)))))
    (if (eq done 'broken)
        nil
      done)))

(defun key-mode-last-open-block-pos (min)
  "Return the position of the last open block, if one found.
Do not move back beyond position MIN."
  (save-excursion
    (let ((count 0))
      (while (not (or (> count 0) (<= (point) min)))
        (key-mode-safe-backward-sexp)
        (setq count
              (cond ((key-mode-at-keyword key-mode-block-start-keywords)
                     (+ count 1))
                    ((and (equal (current-word t) "end")
                          (not (key-mode-in-comment)))
                     (- count 1))
                    (t count))))
      (if (> count 0)
          (point)
        nil))))

(defun key-mode-last-open-block (min)
  "Move back and return indentation level for last open block.
Do not move back beyond MIN."
  ;; Ensure MIN is not before the start of the buffer.
  (setq min (max min (point-min)))
  (let ((pos (key-mode-last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ key-mode-indent-offset (current-indentation))))))

(defsubst key-mode--safe-backward-char ()
  "Move back one character, but don't error if we're at the
beginning of the buffer."
  (unless (eq (point) (point-min))
    (backward-char)))

(defcustom key-mode-max-block-lookback 5000
  "When indenting, don't look back more than this
many characters to see if there are unclosed blocks.
This variable has a moderate effect on indent performance if set too
high, but stops indenting in the middle of long blocks if set too low."
  :type 'integer
  :group 'julia)

(defun key-mode-paren-indent ()
  "Return the column of the text following the innermost
containing paren before point, so we can align succeeding code
with it. Returns nil if we're not within nested parens."
  (save-excursion
    (beginning-of-line)
    (let ((parser-state (syntax-ppss)))
      (cond ((nth 3 parser-state) nil)       ;; strings
            ((= (nth 0 parser-state) 0) nil) ;; top level
            (t
             (ignore-errors ;; return nil if any of these movements fail
               (beginning-of-line)
               (skip-syntax-forward " ")
               (let ((possibly-close-paren-point (point)))
                 (backward-up-list)
                 (let ((open-paren-point (point)))
                   (forward-char)
                   (skip-syntax-forward " ")
                   (if (eolp)
                       (progn
                         (up-list)
                         (backward-char)
                         (let ((paren-closed (= (point) possibly-close-paren-point)))
                           (goto-char open-paren-point)
                           (beginning-of-line)
                           (skip-syntax-forward " ")
                           (+ (current-column)
                              (if paren-closed
                                  0
                                key-mode-indent-offset))))
                     (current-column))))))))))

(defun key-mode-prev-line-skip-blank-or-comment ()
  "Move point to beginning of previous line skipping blank lines
and lines including only comments. Returns number of lines moved.
A return of -1 signals that we moved to the first line of
the (possibly narrowed) buffer, so there is nowhere else to go."
  (catch 'result
    (let ((moved 0) this-move)
      (while t
        (setq this-move (forward-line -1))
        (cond
         ;; moved into comment or blank
         ((and (= 0 this-move)
               (or (looking-at-p "^\\s-*\\(?:#.*\\)*$")
                   (key-mode-in-comment)))
          (incf moved))
         ;; success
         ((= 0 this-move)
          (throw 'result (1+ moved)))
         ;; on first line and in comment
         ((and (bobp)
               (or (looking-at-p "^\\s-*\\(?:#.*\\)*$")
                   (key-mode-in-comment)))
          (throw 'result -1))
         ((bobp)
          (throw 'result moved))
         (t
          (throw 'result 0)))))))

(defun key-mode-indent-hanging ()
  "Calculate indentation for lines that follow \"hanging\"
operators (operators that end the previous line) as defined in
`key-mode-hanging-operator-regexp'. An assignment operator ending
the previous line increases the indent as do the other operators
unless another operator is found two lines up. Previous line
means previous line after skipping blank lines and lines with
only comments."
  (let (prev-indent)
    (save-excursion
      (when (> (key-mode-prev-line-skip-blank-or-comment) 0)
        (setq prev-indent (current-indentation))
        (when (looking-at-p key-mode-hanging-operator-regexp)
          (if (and (> (key-mode-prev-line-skip-blank-or-comment) 0)
                   (looking-at-p key-mode-hanging-operator-regexp))
              ;; two preceding hanging operators => indent same as line
              ;; above
              prev-indent
            ;; one preceding hanging operator => increase indent from line
            ;; above
            (+ key-mode-indent-offset prev-indent)))))))

(defun key-mode-indent-in-string ()
  "Indentation inside strings with newlines is \"manual\",
meaning always increase indent on TAB and decrease on S-TAB."
  (save-excursion
    (beginning-of-line)
    (when (key-mode-in-string)
      (if (member this-command '(key-mode-latexsub-or-indent
                                 ess-indent-or-complete))
          (+ key-mode-indent-offset (current-indentation))
        ;; return the current indentation to prevent other functions from
        ;; indenting inside strings
        (current-indentation)))))

(defun key-mode-indent-import-export-using ()
  "Indent offset for lines that follow `import` or `export`, otherwise nil."
  (when (key-mode-following-import-export-using)
    key-mode-indent-offset))

(defun key-indent-line ()
  "Indent current line of julia code."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation))))
    (indent-line-to
     (or
      ;; note: if this first function returns nil the beginning of the line
      ;; cannot be in a string
      (key-mode-indent-in-string)
      ;; If we're inside an open paren, indent to line up arguments. After this,
      ;; we cannot be inside parens which includes brackets
      (key-mode-paren-indent)
      ;; indent due to hanging operators (lines ending in an operator)
      (key-mode-indent-hanging)
      ;; indent for import and export
      (key-mode-indent-import-export-using)
      ;; Indent according to how many nested blocks we are in.
      (save-excursion
        (beginning-of-line)
        ;; jump out of any comments
        (let ((state (syntax-ppss)))
          (when (nth 4 state)
            (goto-char (nth 8 state))))
        (forward-to-indentation 0)
        (let ((endtok (key-mode-at-keyword key-mode-block-end-keywords))
              (last-open-block (key-mode-last-open-block (- (point) key-mode-max-block-lookback))))
          (max 0 (+ (or last-open-block 0)
                    (if (or endtok
                            (key-mode-at-keyword key-mode-block-start-keywords-no-indent))
                        (- key-mode-indent-offset) 0)))))))
    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

(defalias 'key-mode-mode-prog-mode
  (if (fboundp 'prog-mode)
      'prog-mode
    'fundamental-mode))



(defun key-mode-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst key-mode-syntax-comment-or-string-p (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is inside string or comment."
  (nth 8 (or syntax-ppss (syntax-ppss))))

(defun key-mode-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (key-mode-syntax-comment-or-string-p (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at key-mode-beginning-of-defun-regex))))

(defun key-mode--beginning-of-defun (&optional arg)
  "Internal implementation of `key-mode-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and (not (key-mode-looking-at-beginning-of-defun))
                             ;; f(x) = ... function bodies may span multiple lines
                             (or (and (key-mode-indent-hanging)
                                      (forward-line -1))
                                 ;; inside dangling parameter list
                                 (and (eq 'paren (key-mode-syntax-context-type))
                                      (backward-up-list))
                                 (key-mode-last-open-block (point-min)))))
                 (or (and (key-mode-looking-at-beginning-of-defun)
                          (+ (current-indentation) key-mode-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (key-mode-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 key-mode-beginning-of-defun-regex nil t)
                        (or (key-mode-syntax-comment-or-string-p)
                            ;; handle nested defuns when moving backwards
                            ;; by checking matching indentation
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (key-mode-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) (point))
      (and (goto-char pos) nil))))

(defun key-mode-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled depending on current point position.
Return non-nil (point) if point moved to `beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (key-mode--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun key-mode-end-of-defun (&optional arg)
  "Move point to the end of the current function.
Return nil if point is not in a function, otherwise point."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (key-mode-looking-at-beginning-of-defun)
              (key-mode-beginning-of-defun 1)
              (key-mode-beginning-of-defun -1))
      (beginning-of-line)
      (if (looking-at-p key-mode-function-assignment-regex)
          ;; f(x) = ...
          (progn
            ;; skip any dangling lines
            (while (and (forward-line)
                        (not (eobp))
                        (or (key-mode-indent-hanging)
                            ;; dangling closing paren
                            (and (eq 'paren (key-mode-syntax-context-type))
                                 (search-forward ")"))))))
        ;; otherwise skip forward to matching indentation (not in string/comment)
        (setq beg-defun-indent (current-indentation))
        (while (and (not (eobp))
                    (forward-line 1)
                    (or (key-mode-syntax-comment-or-string-p)
                        (> (current-indentation) beg-defun-indent)))))
      (end-of-line)
      (point))))

(defun key-manual-deindent ()
  "Deindent by `julia-indent-offset' regardless of current
indentation context. To be used to manually indent inside
strings."
  (interactive)
  (indent-line-to (max 0 (- (current-indentation) julia-indent-offset))))
(define-key key-mode-map (kbd "<backtab>") 'key-manual-deindent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Final registration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.key\\'" . key-mode))
(add-to-list 'auto-mode-alist '("\\.key.proof\\'" . key-mode))


(define-derived-mode key-mode prog-mode "KeY"
  "A major mode for editing KeY files"
  :syntax-table key-mode-syntax-table
  ;;:keymap `(,(kbd "TAB") . c-indent-line-or-region)

  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  ;;(set (make-local-variable 'beginning-of-defun-function) #'julia-beginning-of-defun)
  ;;(set (make-local-variable 'end-of-defun-function) #'julia-end-of-defun)
  (setq indent-tabs-mode nil)
  (imenu-add-to-menubar "Imenu")

  (set (make-local-variable 'indent-line-function) 'key-indent-line)
  ;;(set (make-local-variable 'indent-region-function) 'key-indent-region)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (set (make-local-variable 'font-lock-defaults) '(key-mode-font-lock))
  (setq font-lock-keywords key-mode-font-lock)
  (font-lock-flush))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun key-mode--flycheck ()
  (flycheck-define-checker
   key
   "A KeY syntax checker using the fatJar of KeY. To override the
path to the jar file, set `flycheck-key-executable'.  See URL
`http://key-project.org'."
   :command ("pyflakes" source-inplace)
   :error-patterns
   ((error line-start (file-name) ":" line ":" (message) line-end))
   :modes key-mode)

  (add-to-list 'flycheck-checkers 'key))

(eval-after-load 'flycheck #'key-mode--flycheck)
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq key-mode--completions
      `(;; ("keywords" .
	,@(mapcar (lambda (x) (list x "keyword" "")) key-mode--keywords)
	
	;;("sorts"
	("Field" "Sort" "heap.key:4")
	("Heap" "Sort" "heap.key:5")
	("LocSet" "Sort" "locSets.key:4")
	("Map" "Sort" "map.key:19")
	("Pair" "Sort" "wellfound.key:31")
	("Permission" "Sort" "permission.key:15")
	("PermissionOwnerList" "Sort" "permission.key:16")
	("RegEx" "Sort" "regExHeader.key:1")
	("Seq" "Sort" "seq.key:21")
	("\abstract java.io.Serializable \extends java.lang.Object" "Sort" "ruleSetsDeclarations.key:21")
	("\abstract java.lang.Cloneable \extends java.lang.Object" "Sort" "ruleSetsDeclarations.key:20")
	("any" "Sort" "ruleSetsDeclarations.key:18")
	("boolean" "Sort" "boolean.key:14")
	("float, double, real" "Sort" "integerHeader.key:7")
	("int" "Sort" "integerHeader.key:2")
	("java.lang.Object" "Sort" "ruleSetsDeclarations.key:19")
	("numbers" "Sort" "integerHeader.key:1")
	
	;; ("functions" .
	("TRUE" "boolean TRUE" "boolean.key:19")
	("FALSE" "boolean FALSE" "boolean.key:20")
	("clIndexOfChar" "int clIndexOfChar(Seq, int, int)" "charListHeader.key:1")
	("clIndexOfCl" "int clIndexOfCl(Seq,int,Seq)" "charListHeader.key:3")
	("clLastIndexOfChar" "int clLastIndexOfChar(Seq, int, int)" "charListHeader.key:4")
	("clLastIndexOfCl" "int clLastIndexOfCl(Seq, int, Seq)" "charListHeader.key:5")
	("clReplace" "Seq clReplace (Seq, int, int)" "charListHeader.key:7")
	("clTranslateInt" "Seq clTranslateInt(int)" "charListHeader.key:8")
	("clRemoveZeros" "Seq clRemoveZeros(Seq)" "charListHeader.key:9")
	("clHashCode" "int clHashCode(Seq)" "charListHeader.key:10")
	("alpha::select" "alpha alpha::select(Heap, Object, Field)" "heap.key:12")
	("store" "Heap store(Heap, Object, Field, any)" "heap.key:13")
	("create" "Heap create(Heap, Object)" "heap.key:14")
	("anon" "Heap anon(Heap, LocSet, Heap)" "heap.key:15")
	("memset" "Heap memset(Heap, LocSet, any)" "heap.key:16")
	("alpha::defaultValue" "alpha alpha::defaultValue" "heap.key:19")
	("arr" "Field arr(int)" "heap.key:22")
	("java.lang.Object::<transient>" " Field java.lang.Object::<transient>" "heap.key:23")
	("java.lang.Object::<transactionConditionallyUpdated>" " Field java.lang.Object::<transactionConditionallyUpdated>" "heap.key:24")
	("java.lang.Object::<created>" " Field java.lang.Object::<created>" "heap.key:25")
	("Field java.lang.Object::<initialized>" "Field java.lang.Object::<initialized>" "heap.key:26")
	("Field alpha::<classPrepared>" "Field alpha::<classPrepared>" "heap.key:27")
	("Field alpha::<classInitialized>" "Field alpha::<classInitialized>" "heap.key:28")
	("alpha::<classInitializationInProgress>" " Field alpha::<classInitializationInProgress>" "heap.key:29")
	("alpha::<classErroneous>" " Field alpha::<classErroneous>" "heap.key:30")
	("length" "int length(Object)" "heap.key:33")
	("null" "Null null" "heap.key:36")
	("#" "numbers #" "integerHeader.key:19")
	("0" "numbers 0 (numbers)" "integerHeader.key:20")
	("1" "numbers 1 (numbers)" "integerHeader.key:21")
	("2" "numbers 2 (numbers)" "integerHeader.key:22")
	("3" "numbers 3 (numbers)" "integerHeader.key:23")
	("4" "numbers 4 (numbers)" "integerHeader.key:24")
	("5" "numbers 5 (numbers)" "integerHeader.key:25")
	("6" "numbers 6 (numbers)" "integerHeader.key:26")
	("7" "numbers 7 (numbers)" "integerHeader.key:27")
	("8" "numbers 8 (numbers)" "integerHeader.key:28")
	("9" "numbers 9 (numbers)" "integerHeader.key:29")
	("neglit" "numbers neglit (numbers)" "integerHeader.key:30")
	("Z" "int Z (numbers)" "integerHeader.key:32")
	("C" "int C (numbers)" "integerHeader.key:33")
	("add" "int add(int,int)" "integerHeader.key:36")
	("neg" "int neg(int)" "integerHeader.key:37")
	("sub" "int sub(int,int)" "integerHeader.key:38")
	("mul" "int mul(int, int)" "integerHeader.key:39")
	("div" "int div(int, int)" "integerHeader.key:40")
	("mod" "int mod(int, int)" "integerHeader.key:41")
	("pow" "int pow(int, int)" "integerHeader.key:42")
	("bsum{false,false,true}" "int bsum{false,false,true}(int, int, int)" "integerHeader.key:45")
	("bprod{false,false,true}" "int bprod{false,false,true}(int, int, int)" "integerHeader.key:46")
	("sum{true,true}" "int sum{true,true}(boolean,int)" "integerHeader.key:47")
	("prod{true,true}" "int prod{true,true}(boolean,int)" "integerHeader.key:48")
	("min{true,true}" "int min{true,true}(boolean,int)" "integerHeader.key:49")
	("max{true,true}" "int max{true,true}(boolean,int)" "integerHeader.key:50")
	("byte_MAX" "int byte_MAX" "integerHeader.key:53")
	("byte_MIN" "int byte_MIN" "integerHeader.key:54")
	("char_MAX" "int char_MAX" "integerHeader.key:55")
	("char_MIN" "int char_MIN" "integerHeader.key:56")
	("int_MAX" "int int_MAX" "integerHeader.key:57")
	("int_MIN" "int int_MIN" "integerHeader.key:58")
	("long_MAX" "int long_MAX" "integerHeader.key:59")
	("long_MIN" "int long_MIN" "integerHeader.key:60")
	("short_MAX" "int short_MAX" "integerHeader.key:61")
	("short_MIN" "int short_MIN" "integerHeader.key:62")
	("byte_HALFRANGE" "int byte_HALFRANGE" "integerHeader.key:65")
	("byte_RANGE" "int byte_RANGE" "integerHeader.key:66")
	("char_RANGE" "int char_RANGE" "integerHeader.key:67")
	("int_HALFRANGE" "int int_HALFRANGE" "integerHeader.key:68")
	("int_RANGE" "int int_RANGE" "integerHeader.key:69")
	("long_HALFRANGE" "int long_HALFRANGE" "integerHeader.key:70")
	("long_RANGE" "int long_RANGE" "integerHeader.key:71")
	("short_HALFRANGE" "int short_HALFRANGE" "integerHeader.key:72")
	("short_RANGE" "int short_RANGE" "integerHeader.key:73")
	("undefinedPow" "int undefinedPow(int, int)" "integerHeader.key:76")
	("javaUnaryMinusIntOverFlow" "int javaUnaryMinusIntOverFlow(int)" "integerHeader.key:79")
	("javaUnaryMinusLongOverFlow" "int javaUnaryMinusLongOverFlow(int)" "integerHeader.key:80")
	("javaAddIntOverFlow" "int javaAddIntOverFlow(int,int)" "integerHeader.key:81")
	("javaAddLongOverFlow" "int javaAddLongOverFlow(int,int)" "integerHeader.key:82")
	("javaSubIntOverFlow" "int javaSubIntOverFlow(int,int)" "integerHeader.key:83")
	("javaSubLongOverFlow" "int javaSubLongOverFlow(int,int)" "integerHeader.key:84")
	("javaMulIntOverFlow" "int javaMulIntOverFlow(int, int)" "integerHeader.key:85")
	("javaMulLongOverFlow" "int javaMulLongOverFlow(int, int)" "integerHeader.key:86")
	("javaModOverFlow" "int javaModOverFlow(int, int)" "integerHeader.key:87")
	("javaDivIntOverFlow" "int javaDivIntOverFlow(int, int)" "integerHeader.key:88")
	("javaDivLongOverFlow" "int javaDivLongOverFlow(int, int)" "integerHeader.key:89")
	("javaUnsignedShiftRightOverFlow" "int javaUnsignedShiftRightOverFlow(int,int)" "integerHeader.key:90")
	("javaBitwiseOrIntOverFlow" "int javaBitwiseOrIntOverFlow(int,int)" "integerHeader.key:91")
	("javaCastByteOverFlow" "int javaCastByteOverFlow(int)" "integerHeader.key:92")
	("javaCastShortOverFlow" "int javaCastShortOverFlow(int)" "integerHeader.key:93")
	("javaCastIntOverFlow" "int javaCastIntOverFlow(int)" "integerHeader.key:94")
	("javaCastLongOverFlow" "int javaCastLongOverFlow(int)" "integerHeader.key:95")
	("javaCastCharOverFlow" "int javaCastCharOverFlow(int)" "integerHeader.key:96")
	("jmod" "int jmod(int, int)" "integerHeader.key:99")
	("jdiv" "int jdiv(int, int)" "integerHeader.key:100")
	("unaryMinusJint" "int unaryMinusJint(int)" "integerHeader.key:101")
	("unaryMinusJlong" "int unaryMinusJlong(int)" "integerHeader.key:102")
	("addJint" "int addJint(int,int)" "integerHeader.key:103")
	("addJlong" "int addJlong(int,int)" "integerHeader.key:104")
	("subJint" "int subJint(int,int)" "integerHeader.key:105")
	("subJlong" "int subJlong(int,int)" "integerHeader.key:106")
	("mulJint" "int mulJint(int, int)" "integerHeader.key:107")
	("mulJlong" "int mulJlong(int, int)" "integerHeader.key:108")
	("modJint" "int modJint(int, int)" "integerHeader.key:109")
	("modJlong" "int modJlong(int, int)" "integerHeader.key:110")
	("divJint" "int divJint(int, int)" "integerHeader.key:111")
	("divJlong" "int divJlong(int, int)" "integerHeader.key:112")
	("moduloByte" "int moduloByte(int)" "integerHeader.key:113")
	("moduloShort" "int moduloShort(int)" "integerHeader.key:114")
	("moduloInt" "int moduloInt(int)" "integerHeader.key:115")
	("moduloLong" "int moduloLong(int)" "integerHeader.key:116")
	("moduloChar" "int moduloChar(int)" "integerHeader.key:117")
	("shiftright" "int shiftright(/*left*/ int, /*right*/ int)" "integerHeader.key:125")
	("shiftleft" "int shiftleft(/*left*/ int, /*right*/int)" "integerHeader.key:127")
	("unsignedshift" "int unsignedshift(/*left*/ int, /*right*/int, /*bitsize*/ int)" "integerHeader.key:131")
	("shiftrightJint" "int shiftrightJint(int, int)" "integerHeader.key:135")
	("shiftleftJint" "int shiftleftJint(int, int)" "integerHeader.key:136")
	("shiftleftJlong" "int shiftleftJlong(int, int)" "integerHeader.key:138")
	("shiftrightJlong" "int shiftrightJlong(int, int)" "integerHeader.key:139")
	("unsignedshiftrightJint" "int unsignedshiftrightJint(int, int)" "integerHeader.key:141")
	("unsignedshiftrightJlong" "int unsignedshiftrightJlong(int, int)" "integerHeader.key:142")
	("binaryAnd" "int binaryAnd(/*left*/ int, /*right*/ int)" "integerHeader.key:146")
	("binaryOr" "int binaryOr(/*left*/ int, /*right*/ int)" "integerHeader.key:147")
	("binaryXOr" "int binaryXOr(/*left*/ int, /*right*/ int)" "integerHeader.key:148")
	("orJint" "int orJint(int, int)" "integerHeader.key:152")
	("orJlong" "int orJlong(int, int)" "integerHeader.key:153")
	("andJint" "int andJint(int, int)" "integerHeader.key:154")
	("andJlong" "int andJlong(int, int)" "integerHeader.key:155")
	("xorJint" "int xorJint(int, int)" "integerHeader.key:156")
	("xorJlong" "int xorJlong(int, int)" "integerHeader.key:157")
	("index" "int index" "integerHeader.key:162")
	("javaUnaryMinusInt" "int javaUnaryMinusInt(int)" "integerHeader.key:170")
	("javaUnaryMinusLong" "int javaUnaryMinusLong(int)" "integerHeader.key:171")
	("javaBitwiseNegation" "int javaBitwiseNegation(int)" "integerHeader.key:172")
	("javaAddInt" "int javaAddInt(int,int)" "integerHeader.key:173")
	("javaAddLong" "int javaAddLong(int,int)" "integerHeader.key:174")
	("javaSubInt" "int javaSubInt(int,int)" "integerHeader.key:175")
	("javaSubLong" "int javaSubLong(int,int)" "integerHeader.key:176")
	("javaMulInt" "int javaMulInt(int, int)" "integerHeader.key:177")
	("javaMulLong" "int javaMulLong(int, int)" "integerHeader.key:178")
	("javaMod" "int javaMod(int, int)" "integerHeader.key:179")
	("javaDivInt" "int javaDivInt(int, int)" "integerHeader.key:180")
	("javaDivLong" "int javaDivLong(int, int)" "integerHeader.key:181")
	("javaShiftRightInt" "int javaShiftRightInt(int, int)" "integerHeader.key:182")
	("javaShiftRightLong" "int javaShiftRightLong(int, int)" "integerHeader.key:183")
	("javaShiftLeftInt" "int javaShiftLeftInt(int, int)" "integerHeader.key:184")
	("javaShiftLeftLong" "int javaShiftLeftLong(int, int)" "integerHeader.key:185")
	("javaUnsignedShiftRightInt" "int javaUnsignedShiftRightInt(int, int)" "integerHeader.key:186")
	("javaUnsignedShiftRightLong" "int javaUnsignedShiftRightLong(int, int)" "integerHeader.key:187")
	("javaBitwiseOrInt" "int javaBitwiseOrInt(int, int)" "integerHeader.key:188")
	("javaBitwiseOrLong" "int javaBitwiseOrLong(int, int)" "integerHeader.key:189")
	("javaBitwiseAndInt" "int javaBitwiseAndInt(int, int)" "integerHeader.key:190")
	("javaBitwiseAndLong" "int javaBitwiseAndLong(int, int)" "integerHeader.key:191")
	("javaBitwiseXOrInt" "int javaBitwiseXOrInt(int, int)" "integerHeader.key:192")
	("javaBitwiseXOrLong" "int javaBitwiseXOrLong(int, int)" "integerHeader.key:193")
	("javaCastByte" "int javaCastByte(int)" "integerHeader.key:194")
	("javaCastShort" "int javaCastShort(int)" "integerHeader.key:195")
	("javaCastInt" "int javaCastInt(int)" "integerHeader.key:196")
	("javaCastLong" "int javaCastLong(int)" "integerHeader.key:197")
	("javaCastChar" "int javaCastChar(int)" "integerHeader.key:198")
	("empty" "LocSet empty" "locSets.key:9")
	("allLocs" "LocSet allLocs" "locSets.key:10")
	("singleton" "LocSet singleton(Object, Field)" "locSets.key:13")
	("union" "LocSet union(LocSet, LocSet)" "locSets.key:14")
	("intersect" "LocSet intersect(LocSet, LocSet)" "locSets.key:15")
	("setMinus" "LocSet setMinus(LocSet, LocSet)" "locSets.key:16")
	("infiniteUnion{true}" "LocSet infiniteUnion{true}(LocSet)" "locSets.key:17")
	("allFields" "LocSet allFields(Object)" "locSets.key:18")
	("allObjects" "LocSet allObjects(Field)" "locSets.key:19")
	("arrayRange" "LocSet arrayRange(Object, int, int)" "locSets.key:20")
	("freshLocs" "LocSet freshLocs(Heap)" "locSets.key:21")
	("allElementsOfArray" "LocSet allElementsOfArray(Heap, Object, LocSet)" "locSets.key:23")
	("allElementsOfArrayLocsets" "LocSet allElementsOfArrayLocsets(Heap, Object, LocSet)" "locSets.key:24")
	("mapGet" "any mapGet(Map, any)" "map.key:25")
	("mapUndef" "any mapUndef" "map.key:26")
	("mapForeach{true,true}" "Map mapForeach{true,true}(boolean, any)" "map.key:29")
	("mapEmpty" "Map mapEmpty" "map.key:30")
	("mapSingleton" "Map mapSingleton(any, any)" "map.key:31")
	("mapOverride" "Map mapOverride(Map, Map)" "map.key:32")
	("seq2map" "Map seq2map(Seq)" "map.key:33")
	("mapUpdate" "Map mapUpdate(Map, any, any)" "map.key:34")
	("mapRemove" "Map mapRemove(Map, any)" "map.key:35")
	("mapSize" "int mapSize(Map)" "mapSize.key:25")
	("currentThread" "Object currentThread" "permission.key:20")
	("//" " // distinct object representing" "permission.key:20")
	("emptyPermissionOwnerList" "PermissionOwnerList emptyPermissionOwnerList" "permission.key:24")
	("consPermissionOwnerList" "PermissionOwnerList consPermissionOwnerList(Object,PermissionOwnerList)" "permission.key:25")
	("insertPermissionOwner" "PermissionOwnerList insertPermissionOwner(Object,Object,int,PermissionOwnerList)" "permission.key:28")
	("returnPermissionOwner" "PermissionOwnerList returnPermissionOwner(Object, PermissionOwnerList)" "permission.key:29")
	("emptyPermission" "Permission emptyPermission" "permission.key:34")
	("slice" "Permission slice(PermissionOwnerList,Permission)" "permission.key:35")
	("transferPermission" "Permission transferPermission(boolean,Object,Object,int,Permission)" "permission.key:38")
	("returnPermission" "Permission returnPermission(Object,Object,Permission)" "permission.key:39")
	("initFullPermission" "Permission initFullPermission" "permission.key:42")
	("owner1" "PermissionOwnerList owner1(Object)" "permission.key:43")
	("owner2" "PermissionOwnerList owner2(Object,Object)" "permission.key:44")
	("owner3" "PermissionOwnerList owner3(Object,Object,Object)" "permission.key:45")
	("owner4" "PermissionOwnerList owner4(Object,Object,Object,Object)" "permission.key:46")
	("slice1" "Permission slice1(PermissionOwnerList)" "permission.key:48")
	("slice2" "Permission slice2(PermissionOwnerList,PermissionOwnerList)" "permission.key:49")
	("regEx" "RegEx regEx(Seq)" "regExHeader.key:5")
	("opt" "RegEx opt(RegEx)" "regExHeader.key:6")
	("alt" "RegEx alt(RegEx, RegEx)" "regExHeader.key:7")
	("regExConcat" "RegEx regExConcat(RegEx, RegEx)" "regExHeader.key:8")
	("repeat" "RegEx repeat(RegEx, int)" "regExHeader.key:10")
	("repeatStar" "RegEx repeatStar(RegEx)" "regExHeader.key:11")
	("repeatPlus" "RegEx repeatPlus(RegEx)" "regExHeader.key:12")
	("alpha::cast" "alpha alpha::cast(any)" "ruleSetsDeclarations.key:28")
	("alpha::exactInstance" "boolean alpha::exactInstance(any)" "ruleSetsDeclarations.key:29")
	("alpha::instance" "boolean alpha::instance(any)" "ruleSetsDeclarations.key:30")
	("alpha::seqGet" "alpha alpha::seqGet(Seq, int)" "seq.key:31")
	("seqLen" "int seqLen(Seq)" "seq.key:32")
	("seqIndexOf" "int seqIndexOf(Seq, any)" "seq.key:33")
	("seqGetOutside" "any seqGetOutside" "seq.key:34")
	("seqEmpty" "Seq seqEmpty" "seq.key:37")
	("seqSingleton" "Seq seqSingleton(any)" "seq.key:38")
	("seqConcat" "Seq seqConcat(Seq, Seq)" "seq.key:39")
	("seqSub" "Seq seqSub(Seq, int, int)" "seq.key:40")
	("seqReverse" "Seq seqReverse(Seq)" "seq.key:41")
	("seqDef{false,false,true}" "Seq seqDef{false,false,true}(int, int, any)" "seq.key:42")
	("seqSwap" "Seq seqSwap(Seq,int,int)" "seq.key:44")
	("seqRemove" "Seq seqRemove(Seq,int)" "seq.key:45")
	("seqNPermInv" "Seq seqNPermInv(Seq)" "seq.key:46")
	("array2seq" "Seq array2seq(Heap,Object)" "seq.key:48")
	("values" "Seq values" "seq.key:52")
	("seq_def_workaround" "Seq seq_def_workaround(Heap, int, int, Object)" "seq.key:55")
	("seq_def_workaround2" "Seq seq_def_workaround2(Heap, int, int, Object, LocSet)" "seq.key:56")
	("pair" "Pair pair(any, any)" "wellfound.key:36")
	("first" "any first(Pair)" "wellfound.key:37")
	("second" "any second(Pair)" "wellfound.key:38")	
	;;predicates 
	("clStartsWith" "clStartsWith(Seq,Seq)" "charListHeader.key:15")
	("clEndsWith" "clEndsWith(Seq,Seq)" "charListHeader.key:16")
	("clContains" "clContains(Seq,Seq)" "charListHeader.key:17")
	("wellFormed" "wellFormed(Heap)" "heap.key:41")
	("arrayStoreValid" "arrayStoreValid(any, any)" "heap.key:42")
	("nonNull" "nonNull(Heap,Object,int)" "heap.key:43")
	("wellOrderLeqInt" "wellOrderLeqInt(int, int)" "ifThenElseRules.key:36")
	("newObjectsIsomorphic" "newObjectsIsomorphic(Seq, Heap, Seq, Heap)" "infFlow.key:6")
	("newOnHeap" "newOnHeap(Heap, Seq)" "infFlow.key:7")
	("sameTypes" "sameTypes(Seq, Seq)" "infFlow.key:8")
	("sameType" "sameType(any, any)" "infFlow.key:9")
	("objectsIsomorphic" "objectsIsomorphic(Seq, Seq, Seq, Seq)" "infFlow.key:10")
	("objectIsomorphic" "objectIsomorphic(Seq, Object, Seq, Object)" "infFlow.key:11")
	("leq" "leq(int, int)" "integerHeader.key:210")
	("lt" "lt(int, int)" "integerHeader.key:211")
	("geq" "geq(int, int)" "integerHeader.key:212")
	("gt" "gt(int, int)" "integerHeader.key:213")
	("inByte" "inByte(int)" "integerHeader.key:222")
	("inChar" "inChar(int)" "integerHeader.key:223")
	("inInt" "inInt(int)" "integerHeader.key:224")
	("inLong" "inLong(int)" "integerHeader.key:225")
	("inShort" "inShort(int)" "integerHeader.key:226")
	("elementOf" "elementOf(Object, Field, LocSet)" "locSets.key:28")
	("subset" "subset(LocSet, LocSet)" "locSets.key:29")
	("disjoint" "disjoint(LocSet, LocSet)" "locSets.key:30")
	("createdInHeap" "createdInHeap(LocSet, Heap)" "locSets.key:31")
	("inDomain" "inDomain(Map, any)" "map.key:39")
	("inDomainImpliesCreated" "inDomainImpliesCreated(Map)" "map.key:40")
	("isFinite" "isFinite(Map)" "mapSize.key:29")
	("checkPermissionOwner" "checkPermissionOwner(Object,int,PermissionOwnerList)" "permission.key:54")
	("readPermissionObject" "readPermissionObject(Object, Permission)" "permission.key:57")
	("writePermissionObject" "writePermissionObject(Object, Permission)" "permission.key:58")
	("readPermission" "readPermission(Permission)" "permission.key:59")
	("writePermission" "writePermission(Permission)" "permission.key:60")
	("readPermissionOwe" "readPermissionOwe(Object, Object, Permission)" "permission.key:61")
	("readPermissionOwe2" "readPermissionOwe2(Object, Object, Permission)" "permission.key:62")
	("twoPermissions" "twoPermissions(Object,Object,Permission)" "permission.key:66")
	("nonEmptyPermission" "nonEmptyPermission(Permission)" "permission.key:67")
	("permissionsFor" "permissionsFor(Heap,Heap)" "permission.key:69")
	("acc" "acc(Heap, LocSet, Object, Object)" "reach.key:3")
	("reach" "reach(Heap, LocSet, Object, Object, int)" "reach.key:4")
	("match" "match(RegEx, Seq)" "regExHeader.key:16")
	("seqPerm" "seqPerm(Seq,Seq)" "seq.key:25")
	("seqNPerm" "seqNPerm(Seq)" "seq.key:26")
	("prec" "prec(any, any)" "wellfound.key:42")
	("measuredBy" "measuredBy(any)" "wellfound.key:43")
	("measuredByEmpty" "measuredByEmpty" "wellfound.key:44")
	("measuredByCheck" "measuredByCheck(any)" "wellfound.key:45")))

(defun company-key--make-candidate (item)
  (let ((a (first item)) (b (second item)) (c (third item)))    
    (propertize a 'definition b 'source c)))


(defun company-key-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-key-backend))
    (prefix (and (eq major-mode 'key-mode)
	     (company-grab-symbol)))
    
    (candidates
     (mapcar #'company-key--make-candidate
	     (cl-remove-if-not
	      (lambda (c) (string-prefix-p arg (first c)))
	      key-mode--completions)))

    (annotation (format " [%s]"
			(get-text-property 0 'definition arg)))
    
    (meta (format "Definition: %s from %s"
		  (get-text-property 0 'definition arg)
		  (get-text-property 0 'source arg)))))

(defun key-mode--company-support ()
  (add-to-list 'company-backends 'company-key-backend))


(eval-after-load 'company #'key-mode--company-support)


(provide 'key-mode)
;;; key-mode.el ends here

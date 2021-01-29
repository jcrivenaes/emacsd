;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;========================================================================
;; Description:
;;   IRMS editing; based on hacks on ksh-mode and html-helper-mode
;; 
;; Installation:
;;   Put irms.el in some directory in your load-path.
;;   Refer to the installation section of mode's function definition.
;;
;; Blame:
;;   Jan C. Rivenæs (very far from expert in emacs lisp!)
;;
;;========================================================================

(defconst ipl-mode-version "2"
  "*Version numbers of this version of ipl-mode")

;;
;; Variables controlling indentation style
;;
(defvar ipl-indent 4
  "*Indentation of ipl statements with respect to containing block. A value
of nil indicates compound list keyword \(\"do\" and \"then\"\) alignment.")
(defvar ipl-case-item-offset default-tab-width
  "*Additional indentation for case items within a case statement.")
(defvar ipl-case-indent nil
  "*Additional indentation for statements under case items.")
(defvar ipl-group-offset (- 0 default-tab-width)
  "*Additional indentation for keywords \"do\" and \"then\".")
(defvar ipl-brace-offset 0
  "*Additional indentation of \"{\" under functions or brace groupings.")
(defvar ipl-multiline-offset 1
  "*Additional indentation of line that is preceded of a line ending with a
\\ to make it continue on next line.")
(defvar ipl-match-and-tell t
  "*If non-nil echo in the minibuffer the matching compound command
for the \"endwhile\", \"endfor\", \"endif\", or \"other\". ")
(defvar ipl-tab-always-indent t
  "*Controls the operation of the TAB key. If t (the default), always
reindent the current line.  If nil, indent the current line only if
point is at the left margin or in the line's indentation; otherwise
insert a tab.")

(defvar ipl-align-to-keyword t
  "*Controls whether nested constructs align from the keyword or
the current indentation. If non-nil, indentation will be relative to
the column the keyword starts. If nil, indentation will be relative to
the current indentation of the line the keyword is on.
The default value is non-nil.")

(defvar ipl-comment-regexp "^\\s */"
  "*Regular expression used to recognize comments. Customize to support
ipl-like languages.")

(defun ipl-current-indentation ()
  nil
  )
;;
(fset 'ipl-current-indentation 'current-column)
;;
;; Variables controlling completion
(defvar ipl-completion-list '())
(make-variable-buffer-local 'ipl-completion-list)
(set-default 'ipl-completion-list  '())

;;
;; -type-  : type number, 0:misc, 1:variable, 2:function
;; -regexp-: regexp used to parse the script
;; -match- : used by match-beginning/end to pickup target
;;
(defvar ipl-completion-type-misc 0)
(defvar ipl-completion-regexp-var "\\([A-Za-z_0-9]+\\)=")
(defvar ipl-completion-type-var 1)
(defvar ipl-completion-match-var 1) 
(defvar ipl-completion-regexp-var2 "\\$\\({\\|{#\\)?\\([A-Za-z_0-9]+\\)[#%:}]?")
(defvar ipl-completion-match-var2 2)
(defvar ipl-completion-regexp-function
  "\\(function\\)?[ \t]*\\([A-Za-z_0-9]+\\)[ \t]*([ \t]*)")
(defvar ipl-completion-type-function 2)
(defvar ipl-completion-match-function 2)

;;
;; Variable controlling fontification?
;;
;; (defvar ipl-keywords '("for" "from" "do" "endfor" "popup" "endpopup" 
;; "group" "endgroup" "endwhile" "if" "then" "elseif" "else" "endif" 
;; "while" "function" "endfunction" "runtime" "endruntime"))

;; Context/indentation regular expressions
;; 
;; indenting expressions
;;
(defconst ipl-then-do-re     "^[^/'`\"\n]*\\b\\(then\\|do\\)\\b"
  "*Regexp used to locate grouping keywords: \"then\" and \"do\"" )

;;(defconst ipl-do-re          "^[ \t]*\\bdo\\(\\b\\|$\\)"
(defconst ipl-do-re          "^\\s *\\bdo\\(\\b\\|$\\)"
  "*Regexp used to match keyword: do")

(defconst ipl-then-re        "^\\s *\\bthen\\(\\b\\|$\\)"
  "*Regexp used to match keyword: then")

(defconst ipl-from-re        "^\\s *\\bfrom\\(\\b\\|$\\)"
  "*Regexp used to match keyword: from")

;;
;; Structure starting/indenting keywords
;;
(defconst ipl-else-re           "^\\s *\\belse\\(\\b\\|$\\)"
  "*Regexp used to match keyword: else")

(defconst ipl-elseif-re           "^\\s *\\belseif\\(\\b\\|$\\)"
  "*Regexp used to match keyword: elseif")

(defconst ipl-brace-re           "^\\S>*{[ \t\n]"
  "*Regexp used to match syntactic entity: { ")

;; (defconst ipl-case-item-end-re           "^\\S>*;;[ \t\n]"
;;   "*Regexp used to match case item end syntactic entity: ;;")

(defconst ipl-case-item-end-re           "^\\S>*;;[ \t\n]"
  "*Regexp used to match case item end syntactic entity: ;;")

(defconst ipl-esac-re          "^\\s *esac\\b"
  "*Regexp used to match keyword: esac")

(defconst ipl-keywords-re
  "^[^/'`\"\n]*\\b\\(else\\|if\\|elseif\\|while\\|for\\|group\\|popup\\|function\\|runtime\\)\\b"
  "*Regexp used to detect compound command keywords: if, else, elseif, 
while, for, group, popup, function")

(defconst ipl-keywords-re-ifspecial
  "^[^/'`\"\n]*\\b\\(if\\)\\b.+\\b\\(endif\\)\\b"
  "*Regexp used to detect compound command keywords: if endif on same 
line")

(defconst ipl-if-re         "^[^/'`\"\n]*\\b\\(if\\)\\b"
  "*Regexp used to match keyword: if")

;; (defconst ipl-if-re         "^[^/'`\"\n]*\\b\\(if\\)\\b.+([^e]|e[^n]|en[^d]|end[^i]|endi[^f])*$"
;;   "*Regexp used to match keyword: if")

(defconst ipl-if-endif-re         "^[^/'`\"\n]*\\b\\(if\\)\\b.+\\b\\(endif\\)\\b"
  "*Regexp used to match keyword: if endif one liner")

(defconst ipl-while-key-re 
  "^[^/'`\"\n]*\\b\\(while\\)\\b"
  "*Match the keywords: while")

(defconst ipl-for-key-re 
  "^[^/'`\"\n]*\\b\\(for\\)\\b"
  "*Match the keywords: for")

(defconst ipl-group-key-re 
  "^[^/'`\"\n]*\\b\\(group.*\\)\\b"
  "*Match the keywords: group")

(defconst ipl-runtime-key-re 
  "^[^/'`\"\n]*\\b\\(runtime\\)\\b"
  "*Match the keywords: runtime")

(defconst ipl-popup-key-re 
  "^[^/'`\"\n]*\\b\\(popup\\)\\b"
  "*Match the keywords: popup")

(defconst ipl-funkeys-re 
  "^[^/'`\"\n]*\\b\\(function\\)\\b"
  "*Match one of the keywords: float, int, ... for function")

(defconst ipl-case-re           "^[^#'`\"\n]*\\b\\(case\\)\\b"
  "*Regexp used to match keyword: case")

(defconst ipl-explicit-func-re
  "^\\s *\\(function\\s [a-zA-z_][a-zA-Z0-1_]*\\)\\b"
  "*Match an explicit function definition: function name")


;; JRIV: note to fix the problem with lack of indent after Popup(), added xxx...
;; (defconst ipl-implicit-func-re
;;   "^\\s *\\([a-zA-z_][a-zA-Z0-1_]*\\)\\s *()\\s *"   
;;   "*Match an implicit function definition: name ()")

(defconst ipl-implicit-func-re
  "^\\s *\\([a-zA-z_][a-zA-Z0-1_]*\\)\\s *(xxx)\\s *"   
  "*Match an implicit function definition: name ()")

(defconst ipl-func-brace-re "^\\s *\\(.*{\\)[ \t\n]+"
  "*Match a implicit function definition brace: name { ")

;;
;; structure ending keyword regular expressions
(defconst ipl-endif-re            "^\\s *endif\\b"
  "*Regexp used to match keyword: endif")

(defconst ipl-endfor-re          "^\\s *endfor\\b"
  "*Regexp used to match keyword: endfor")

(defconst ipl-endwhile-re          "^\\s *endwhile\\b"
  "*Regexp used to match keyword: endwhile")

(defconst ipl-endpopup-re          "^\\s *endpopup\\b"
  "*Regexp used to match keyword: endpopup")

(defconst ipl-endfunction-re          "^\\s *endfunction\\b"
  "*Regexp used to match keyword: endfunction")

(defconst ipl-endgroup-re          "^\\s *endgroup\\b"
  "*Regexp used to match keyword: endgroup")

(defconst ipl-endruntime-re          "^\\s *endruntime"
  "*Regexp used to match keyword: endruntime")


;;
;; indenting 
(defconst ipl-case-item-re           "^[^/`'\"\n]*\\(\\)"
  "*Regexp used to match case-items including ipl88")

(defconst ipl-paren-re           "^[^/`'\"\n]*)[ \t\n]+"
  "*Regexp used to match compound list & case items")


(defconst ipl-brace-end-re  "^\\s *}\\s *"
  "*Regexp used to match function brace-groups")

(defconst ipl-multiline-re "^.*\\\\$"
  "*Regexp used to match a line with a statement using more lines.")

;;
;;
;; Create mode specific tables
(defvar ipl-mode-syntax-table nil
  "Syntax table used while in ipl mode.")
(if ipl-mode-syntax-table
    ()
  (setq ipl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" ipl-mode-syntax-table)
  (modify-syntax-entry ?` "\"" ipl-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " ipl-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " ipl-mode-syntax-table)
  (modify-syntax-entry ?/ "<   " ipl-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ipl-mode-syntax-table)
  (modify-syntax-entry ?< "." ipl-mode-syntax-table)
  (modify-syntax-entry ?> "." ipl-mode-syntax-table)
  (modify-syntax-entry ?& "." ipl-mode-syntax-table)
  (modify-syntax-entry ?| "." ipl-mode-syntax-table)
  (modify-syntax-entry ?$ "." ipl-mode-syntax-table)
  (modify-syntax-entry ?% "." ipl-mode-syntax-table)
  (modify-syntax-entry ?= "." ipl-mode-syntax-table)
  (modify-syntax-entry ?/ "." ipl-mode-syntax-table)
  (modify-syntax-entry ?+ "." ipl-mode-syntax-table)
  (modify-syntax-entry ?* "." ipl-mode-syntax-table)
  (modify-syntax-entry ?- "." ipl-mode-syntax-table)
  (modify-syntax-entry ?; "." ipl-mode-syntax-table)
  )

(defvar ipl-mode-abbrev-table nil
  "Abbrev table used while in ipl mode.")
(define-abbrev-table 'ipl-mode-abbrev-table ())

(defvar ipl-mode-map nil 
  "Keymap used in ipl mode")

(if ipl-mode-map
    ()
  (setq ipl-mode-map (make-sparse-keymap))
  (define-key ipl-mode-map "\t"    'ipl-indent-command)
;;  (define-key ipl-mode-map "\t"    'ipl-indent-line)
  (define-key ipl-mode-map "\177"    'backward-delete-char-untabify)
  (define-key ipl-mode-map "\C-j"    'reindent-then-newline-and-indent)
  (define-key ipl-mode-map "\e\t"    'ipl-complete-symbol)
  (define-key ipl-mode-map "\C-c\t"    'ipl-completion-init-and-pickup)
  )


(defun ipl-mode ()
  "ipl-mode 2.0 - Major mode for editing IPL scripts.
Special key bindings and commands:
\\{ipl-mode-map}
Variables controlling indentation style:
ipl-indent
    Indentation of ipl statements with respect to containing block.
    Default value is 8.
ipl-case-indent
    Additional indentation for statements under case items.
    Default value is nil which will align the statements one position 
    past the \")\" of the pattern.
ipl-case-item-offset
    Additional indentation for case items within a case statement.
    Default value is 8.
ipl-group-offset
    Additional indentation for keywords \"do\" and \"then\".
    Default value is -8.
ipl-brace-offset
    Additional indentation of \"{\" under functions or brace groupings.
    Default value is 0.
ipl-multiline-offset
   Additional indentation of line that is preceded of a line ending with a
   \\ to make it continue on next line.
ipl-tab-always-indent
    Controls the operation of the TAB key. If t (the default), always
    reindent the current line.  If nil, indent the current line only if
    point is at the left margin or in the line's indentation; otherwise
    insert a tab.
ipl-match-and-tell
    If non-nil echo in the minibuffer the matching compound command
    for the \"endwhile\", \"}\", \"endif\", or \"endfor\". Default value is t.

ipl-align-to-keyword
    Controls whether nested constructs align from the keyword or
    the current indentation. If non-nil, indentation will be relative to
    the column the keyword starts. If nil, indentation will be relative to
    the current indentation of the line the keyword is on.
    The default value is non-nil.

ipl-comment-regexp
  Regular expression used to recognize comments. Customize to support
  ipl-like languages. Default value is \"\^\\\\s */\".

Style Guide.
 By setting
    (setq ipl-indent default-tab-width)
    (setq ipl-group-offset 0)

    The following style is obtained:

    if [ -z $foo ]
	    then
		    bar    # <-- ipl-group-offset is additive to ipl-indent
		    foo
    endif

 By setting
    (setq ipl-indent default-tab-width)
    (setq ipl-group-offset (- 0 ipl-indent))

    The following style is obtained:

    if [ -z $foo ]
    then
	    bar
	    foo
    endif

 By setting
    (setq ipl-case-item-offset 1)
    (setq ipl-case-indent nil)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ipl-case-item-offset
          baz;;         # <-- ipl-case-indent aligns with \")\"
     foobar) foo
             bar;;
    esac

 By setting
    (setq ipl-case-item-offset 1)
    (setq ipl-case-indent 6)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ipl-case-item-offset
           baz;;        # <-- ipl-case-indent
     foobar) foo
           bar;;
    esac
    

Installation:
  Put ipl-mode.el in some directory in your load-path.
  Put the following forms in your .emacs file.

 (autoload 'ipl-mode \"ipl-mode\" \"Major mode for editing IPL Scripts.\" t)

 (setq auto-mode-alist
      (append auto-mode-alist
	      (list
	       '(\"\\\\.ipl$\" . ipl-mode)
               '(\"\\\\.exe\" . ipl-mode)
               '(\"\\\\..*iraprms\" . ipl-mode))))

 (setq ipl-mode-hook
      (function (lambda ()
         (setq ipl-indent 8)
	 (setq ipl-group-offset -8))
	 (setq ipl-brace-offset -8)   
         (setq ipl-tab-always-indent t)
         (setq ipl-match-and-tell t)
         (setq ipl-align-to-keyword t)	;; Turn on keyword alignment
	 )))

"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ipl-mode-map)
  (setq major-mode 'ipl-mode)
  (setq mode-name "IPL mode by JRIV - Statoil")
  (setq font-lock-defaults '(ipl-font-lock-keywords))
  (setq mode-name "RMS IPL Mode by JRIV - Statoil")
  (font-lock-mode 1)
  (setq local-abbrev-table ipl-mode-abbrev-table)
  (set-syntax-table ipl-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ipl-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'ipl-indent-region)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//+ *")
  ;;
  ;;
  ;; Let the user customize
  (run-hooks 'ipl-mode-hook)
  (if (not ipl-align-to-keyword)
      (ipl-align-to-keyword -1)
    )
  ) ;; defun

;;
;; Support functions

(defun ipl-align-to-keyword (&optional arg)
  "Toggle value of ipl-align-to-keyword and rebind the ipl-current-indentation
function. With arg, force alignment to keyword if and only if arg is positive."
  (interactive)
  (if (null arg)			;just toggle
      (cond ((not ipl-align-to-keyword)
	     (setq ipl-align-to-keyword t)
	     (fset 'ipl-current-indentation 'current-column))
	    (t
	     (setq ipl-align-to-keyword nil)
	     (fset 'ipl-current-indentation 'current-indentation))
	    )
    (cond ((natnump arg)
	   (setq ipl-align-to-keyword t)
	   (fset 'ipl-current-indentation 'current-column))
	  (t
	   (setq ipl-align-to-keyword nil)
	   (fset 'ipl-current-indentation 'current-indentation))
	  ))
  )

(defun ipl-current-line ()
  "Return the vertical position of point in the buffer.
Top line is 1."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0))
  )


(defun ipl-line-to-string ()
  "From point, construct a string from all characters on
current line"
  (skip-chars-forward " \t") ;; skip tabs as well as spaces
  (buffer-substring (point)
                    (progn
                      (end-of-line 1)
                      (point))))

(defun ipl-get-nest-level ()
  "Return a 2 element list (nest-level nest-line) describing where the
current line should nest."
  (let (
    	(level))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp))
		  (null level))
	(if (and (not (looking-at "^\\s *$"))
 		 (not (save-excursion
 			(forward-line -1)
 			(beginning-of-line)
			(looking-at ipl-multiline-re)))
		 (not (looking-at ipl-comment-regexp)))
	    (setq level (cons (current-indentation)
			      (ipl-current-line)))
	  (forward-line -1)
	  );; if
	);; while
      (if (null level)
	  (cons (current-indentation) (ipl-current-line))
	level)
      )
    )
  )

(defun ipl-looking-at-compound-list ()
  "Return true if current line contains compound list initiating keyword"
  (or 
   (looking-at ipl-do-re)
   (looking-at ipl-then-re)
   ) ;; or
  ) ;; defun


(defun ipl-looking-at-case-item ()
  "Return true if current line is a case-item .vs. paren compound list"
  (save-excursion
    (beginning-of-line)
    ;;
    ;; Handle paren indentation constructs for this line
    (cond ((looking-at ipl-paren-re)
    	   (goto-line (cdr (ipl-get-nest-level)))
    	   ;;
    	   ;; The question is whether this is really a case item or just
    	   ;; parenthesized compound list.
    	   (cond ((or (looking-at ipl-case-re)
    		      (looking-at ipl-case-item-end-re)))
    		 ;;
    		 ;; turns out to be a parenthesized compound list
    		 ;; so propigate the nil for cond
    		 )
    	   ))
    )
  ) ;; defun

(defun ipl-get-case-indent ()
  "Return the column of the closest open case statement"
  (save-excursion
    (let (
	  (nest-list (ipl-get-compound-level ipl-case-re ipl-esac-re (point)))
	  )
      (if (null nest-list)
	  (progn 
	    (if ipl-match-and-tell
		(message "No matching case for ;;"))
	    0)
	(car nest-list)))
    )
  )


;;
;; Functions which make this mode what it is
;;

(defun ipl-get-nester-column (nest-line)
  "Return the column to indent to with respect to nest-line taking 
into consideration keywords and other nesting constructs."
  (save-excursion 
    (let ((fence-post)
	  (nester-column)
	  (start-line (ipl-current-line)))
      ;;
      ;; Handle case item indentation constructs for this line
      (cond ((ipl-looking-at-case-item)
	     (save-excursion
	       (goto-line nest-line)
	       (let ((fence-post (save-excursion (end-of-line) (point))))
		 ;;
		 ;; Now know there is a case-item so detect whether
		 ;; it is first under case, just another case-item, or
		 ;; a case-item and case-item-end all rolled together.
		 ;;
		 (cond ((re-search-forward ipl-case-re fence-post t)
			(goto-char (match-beginning 1))
			(+ (ipl-current-indentation) ipl-case-item-offset))

		       ((ipl-looking-at-case-item)
			(current-indentation))

		       ((looking-at ipl-case-item-end-re)
			(end-of-line)
			(+ (ipl-get-case-indent) ipl-case-item-offset))
		       )
		 )))
	    (t;; Not a case-item.  What to do relative to the nest-line?
	     (save-excursion
	       (goto-line nest-line)
	       (setq fence-post (save-excursion (end-of-line) (point)))
	       (setq nester-column
		     (save-excursion
		       (cond
			;;
			;; Check if we are in a continued statement
			((and (looking-at ipl-multiline-re)
			      (save-excursion
				(goto-line (1- start-line))
				(looking-at ipl-multiline-re)))
			 (+ (current-indentation) ipl-multiline-offset))

			;; In order to locate the column of the keyword,
			;; which might be embedded within a case-item,
			;; it is necessary to use re-search-forward.
			((re-search-forward ipl-keywords-re fence-post t)
			 (goto-char (match-beginning 1))
			 (if (looking-at ipl-case-re)
			     (+ (ipl-current-indentation) ipl-case-item-offset)
			   (+ (ipl-current-indentation)
			      (if (null ipl-indent)
				  2 ipl-indent)
			      )))


			((re-search-forward ipl-then-do-re fence-post t)
			 (if (null ipl-indent)
			     (progn 
			       (goto-char (match-end 1))
			       (+ (ipl-current-indentation) 1))
			   (progn
			     (goto-char (match-beginning 1))
			     (+ (ipl-current-indentation) ipl-indent))
			   ))

			((looking-at ipl-brace-re)
			 (+ (current-indentation)
			    (if (null ipl-indent)
				2 ipl-indent)
			    ))
			;;
			;; Forces functions to first column
			((or (looking-at ipl-implicit-func-re)
			     (looking-at ipl-explicit-func-re))
			 (if (looking-at ipl-func-brace-re)
			     (if (null ipl-indent)
				 2 ipl-indent)
			   ipl-brace-offset))

			;;
			;; Need to first detect the end of a case-item
			((looking-at ipl-case-item-end-re)
			 (end-of-line)
			 (+ (ipl-get-case-indent) ipl-case-item-offset))
			;;
			;; Now detect first statement under a case item
			((ipl-looking-at-case-item)
			 (if (null ipl-case-indent)
			     (progn
			       (re-search-forward ipl-case-item-re fence-post t)
			       (goto-char (match-end 1))
			       (+ (current-column) 1))
			   (+ (current-indentation) ipl-case-indent)))

			;; This is hosed when using current-column
			;; and there is a multi-command expression as the
			;; nester.
			(t (current-indentation)))
		       )
		     ));; excursion over
	     ;;
	     ;; Handle additional indentation constructs for this line
	     (cond ((ipl-looking-at-compound-list)
		    (+ nester-column ipl-group-offset))
		   ((looking-at ipl-brace-re)
		    (+ nester-column ipl-brace-offset))
		   (t nester-column))
	     );; Not a case-item
	    )
      );;let

    ;; ((re-search-forward ipl-keywords-re-ifspecial fence-post t)
    ;;  (goto-char (match-beginning 1))
    ;;  (if (looking-at ipl-case-re)
    ;; 	 (+ (ipl-current-indentation) ipl-case-item-offset)
    ;;    (nester-column
    ;; 	(if (null ipl-indent)
    ;; 	    2 ipl-indent)
    ;; 	)))
    );; excursion
  );; defun

(defun ipl-indent-command ()
  "Indent current line relative to containing block and allow for
ipl-tab-always-indent customization"
  (interactive)
  (cond ((save-excursion
	   (skip-chars-backward " \t")
	   (bolp))
	 (ipl-indent-line))
	(ipl-tab-always-indent
	 (save-excursion
	   (ipl-indent-line)))
	(t (insert-tab))
	)
  )


(defun ipl-indent-line ()
  "Indent current line as far as it should go according
to the syntax/context"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        nil
      ;;
      ;; Align this line to current nesting level
      (let*
          (
	   (level-list (ipl-get-nest-level))     ; Where to nest against
;;           (last-line-level (car level-list))
           (this-line-level (current-indentation))
	   (nester-column (ipl-get-nester-column (cdr level-list)))
	   (struct-match (ipl-match-structure-and-reindent))
	   )
	(if struct-match
	    (setq nester-column struct-match))
	(if (eq nester-column this-line-level)
	    nil
	  (beginning-of-line)
	  (let ((beg (point)))
	    (back-to-indentation)
	    (delete-region beg (point)))
	  (indent-to nester-column))
	) ;; let*
      ) ;; if
    ) ;; excursion
  ;;
  ;; Position point on this line
  (let*
      (
       (this-line-level (current-indentation))
       (this-bol (save-excursion
                   (beginning-of-line)
                   (point)))
       (this-point (- (point) this-bol))
       )
    (cond ((> this-line-level this-point) ;; point in initial white space
           (back-to-indentation))
           (t nil)
           ) ;; cond
    ) ;; let*
  ) ;; defun


(defun ipl-match-indent-level (begin-re end-re)
  "Match the compound command and indent. Return nil on no match,
indentation to use for this line otherwise."
  (interactive)
  (let* (
	 (nest-list 
	  (save-excursion
	    (ipl-get-compound-level begin-re end-re (point))
	    ))
	 ) ;; bindings
    (if (null nest-list)
	(progn
	  (if ipl-match-and-tell
	      (message "No matching compound command"))
	  nil) ;; Propagate a miss.
      (let* (
	     (nest-level (car nest-list))
	     (match-line (cdr nest-list))
	     ) ;; bindings
	(if ipl-match-and-tell
	    (save-excursion
	      (goto-line match-line)
	      (message "Matched ... %s" (ipl-line-to-string))
	      ) ;; excursion
	  ) ;; if ipl-match-and-tell
	nest-level ;;Propagate a hit.
	) ;; let*
      ) ;; if
    ) ;; let*
  ) ;; defun ipl-match-indent-level

(defun ipl-match-structure-and-reindent ()
  "If the current line matches one of the indenting keywords
or one of the control structure ending keywords then reindent. Also
if ipl-match-and-tell is non-nil the matching structure will echo in
the minibuffer"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at ipl-else-re)
	  (ipl-match-indent-level ipl-if-re ipl-endif-re))
	  ((looking-at ipl-elseif-re)
	   (ipl-match-indent-level ipl-if-re ipl-endif-re))
	  ((looking-at ipl-endif-re)
	   (ipl-match-indent-level ipl-if-re ipl-endif-re))
	  ;; does not work (jriv):
	  ((looking-at ipl-if-endif-re)
	   (- ipl-indent))
          ((looking-at ipl-endwhile-re)
	   (ipl-match-indent-level ipl-while-key-re ipl-endwhile-re))
	  ((looking-at ipl-endfor-re)
	   (ipl-match-indent-level  ipl-for-key-re ipl-endfor-re))
	  ((looking-at ipl-endpopup-re)
	   (ipl-match-indent-level  ipl-popup-key-re ipl-endpopup-re))
	  ((looking-at ipl-endgroup-re)
	   (ipl-match-indent-level  ipl-group-key-re ipl-endgroup-re))
	  ((looking-at ipl-endruntime-re)
	   (ipl-match-indent-level  ipl-runtime-key-re ipl-endruntime-re))
	  ((looking-at ipl-endfunction-re)
	   (ipl-match-indent-level  ipl-funkeys-re ipl-endfunction-re))
	  ;;
	  ((looking-at ipl-brace-end-re)
	   (cond
	    ((ipl-match-indent-level ipl-implicit-func-re ipl-brace-end-re))
	    ((ipl-match-indent-level ipl-explicit-func-re ipl-brace-end-re))
	    ((ipl-match-indent-level ipl-func-brace-re ipl-brace-end-re))
	    (t nil)))
	  (t nil)
	  );; cond
    )
  )

(defun ipl-get-compound-level 
  (begin-re end-re anchor-point &optional balance-list)
  "Determine how much to indent this structure. Return a list (level line) 
of the matching compound command or nil if no match found."
  (let* 
      (;; Locate the next compound begin keyword bounded by point-min
       (match-point (if (re-search-backward begin-re (point-min) t)
			(match-beginning 1) 0))
       (nest-column (if (zerop match-point)
			1 
		      (progn
			(goto-char match-point)
			(ipl-current-indentation))))
       (nest-list (cons 0 0))    ;; sentinel cons since cdr is >= 1
       )
    (if (zerop match-point)
	nil ;; graceful exit from recursion
      (progn
	(if (nlistp balance-list)
	    (setq balance-list (list)))
	;; Now search forward from matching start keyword for end keyword
	(while (and (consp nest-list) (zerop (cdr nest-list))
		    (re-search-forward end-re anchor-point t))
	  (if (not (memq (point) balance-list))
	      (progn
		(setq balance-list (cons (point) balance-list))
		(goto-char match-point)  ;; beginning of compound cmd
		(setq nest-list
		      (ipl-get-compound-level begin-re end-re
					     anchor-point balance-list))
		)))

	(cond ((consp nest-list)
	       (if (zerop (cdr nest-list))
		 (progn
		   (goto-char match-point)
		   (cons nest-column (ipl-current-line)))
		 nest-list))
	      (t nil)
	      )
	)
      )
    )
  )


(defun ipl-indent-region (start end)
  "From start to end, indent each line."
  ;; The algorithm is just moving through the region line by line with
  ;; the match noise turned off.  Only modifies nonempty lines.
  (save-excursion
    (let (ipl-match-and-tell
	  (endmark (copy-marker end)))
      
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (while (> (marker-position endmark) start)
	(if (not (and (bolp) (eolp)))
	    (ipl-indent-line))
	(forward-line 1)
	(setq start (point)))

      (set-marker endmark nil)
      )
    )
  )

;;
;; Completion code supplied by Haavard Rue <hrue@imf.unit.no>.
;;
;;
;; add a completion with a given type to the list
;;
(defun ipl-addto-alist (completion type)
  (setq ipl-completion-list
	(append ipl-completion-list
		(list (cons completion type)))))
;;
;; init the list and pickup all 
;;
(defun ipl-completion-init-and-pickup ()
  (interactive)
  (ipl-completion-list-init)
  (ipl-pickup-all))

;;
;; init the list
;;
(defun ipl-completion-list-init ()
  (interactive)
  (setq ipl-completion-list
	(list
	 (cons "if"  ipl-completion-type-misc)
	 (cons "while"  ipl-completion-type-misc)
	 (cons "group"  ipl-completion-type-misc)
	 (cons "popup"  ipl-completion-type-misc)
	 (cons "endgroup"  ipl-completion-type-misc)
	 (cons "function"  ipl-completion-type-misc)
	 (cons "endfunction"  ipl-completion-type-misc)
	 (cons "endpopup"  ipl-completion-type-misc)
	 (cons "for"  ipl-completion-type-misc)
	 (cons "continue"  ipl-completion-type-misc)
	 (cons "endif"  ipl-completion-type-misc)
	 (cons "endfor"  ipl-completion-type-misc)
	 (cons "exit"  ipl-completion-type-misc)
	 (cons "endwhile"  ipl-completion-type-misc)
	 (cons "do"  ipl-completion-type-misc))))

(defun ipl-eol-point ()
  (save-excursion
    (end-of-line)
    (point)))

(defun ipl-bol-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun ipl-pickup-all ()
  "Pickup all completions in buffer."
  (interactive)
  (ipl-pickup-completion-driver (point-min) (point-max) t))

(defun ipl-pickup-this-line ()
  "Pickup all completions in current line."
  (interactive)
  (ipl-pickup-completion-driver (ipl-bol-point) (ipl-eol-point) nil))

(defun ipl-pickup-completion-driver (pmin pmax message)
  "Driver routine for ipl-pickup-completion."
  (if message
      (message "pickup completion..."))
  (let* (
	 (i1
	  (ipl-pickup-completion  ipl-completion-regexp-var
				 ipl-completion-type-var
				 ipl-completion-match-var
				 pmin pmax))
	 (i2
	  (ipl-pickup-completion  ipl-completion-regexp-var2
				 ipl-completion-type-var
				 ipl-completion-match-var2
				 pmin pmax))
	 (i3
	  (ipl-pickup-completion  ipl-completion-regexp-function
				 ipl-completion-type-function
				 ipl-completion-match-function
				 pmin pmax)))
    (if message
	(message "pickup %d variables and %d functions." (+ i1 i2) i3))))

(defun ipl-pickup-completion (regexp type match pmin pmax)
  "Pickup completion in region and addit to the list, if not already
there." 
  (let ((i 0) kw obj)
    (save-excursion
      (goto-char pmin)
      (while (and
	      (re-search-forward regexp pmax t)
	      (match-beginning match)
	      (setq kw  (buffer-substring
			 (match-beginning match)
			 (match-end match))))
	(progn
	  (setq obj (assoc kw ipl-completion-list))
	  (if (or (equal nil obj)
		  (and (not (equal nil obj))
		       (not (= type (cdr obj)))))
	      (progn
		(setq i (1+ i))
		(ipl-addto-alist kw type))))))
    i))

(defun ipl-complete-symbol ()
  "Perform completion."
  (interactive)
  (let* ((end (point))
         (beg (unwind-protect
                  (save-excursion
                    (backward-sexp 1)
                    (while (= (char-syntax (following-char)) ?\')
                      (forward-char 1))
                    (point))))
         (pattern (buffer-substring beg end))
	 (predicate 
	  ;;
	  ;; ` or $( mark a function
	  ;;
	  (save-excursion
	    (goto-char beg)
	    (if (or
		 (save-excursion
		   (backward-char 1)
		   (looking-at "`"))
		 (save-excursion
		   (backward-char 2)
		   (looking-at "\\$(")))
		(function (lambda (sym)
			    (equal (cdr sym) ipl-completion-type-function)))
	      ;;
	      ;; a $, ${ or ${# mark a variable
	      ;;
	      (if (or
		   (save-excursion
		     (backward-char 1)
		     (looking-at "\\$"))
		   (save-excursion
		     (backward-char 2)
		     (looking-at "\\${"))
		   (save-excursion
		     (backward-char 3)
		     (looking-at "\\${#")))
		  (function (lambda (sym)
			      (equal (cdr sym)
				     ipl-completion-type-var)))
		;;
		;; don't know. use 'em all
		;;
		(function (lambda (sym) t))))))
	 ;;
	 (completion (try-completion pattern ipl-completion-list predicate)))
    ;;
    (cond ((eq completion t))
	  ;;
	  ;; oops, what is this ?
	  ;;
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern))
	  ;;
	  ;; insert
	  ;;
          ((not (string= pattern completion))
           (delete-region beg end)
           (insert completion))
	  ;;
	  ;; write possible completion in the minibuffer,
	  ;; use this instead of a seperate buffer (usual)
	  ;;
          (t
           (let ((list (all-completions pattern ipl-completion-list predicate))
		 (string ""))
	     (while list
	       (progn
		 (setq string (concat string (format "%s " (car list))))
		 (setq list (cdr list))))
	     (message string))))))



;; ======================================================================
;; The colours
;; ======================================================================


;; This is basically a list of lists with a regular expression and
;; then the font name (or face name). The numbers below refer to \\(
;; ... \\) constructs that mark what part of the re you want
;; highlighted 

;; redefine or define face names

;; comment face
(copy-face             'default 'font-lock-comment-face)
(set-face-foreground         'font-lock-comment-face "DarkGoldenrod")
(setq font-lock-comment-face 'font-lock-comment-face)


;; string face
(set-face-foreground         'font-lock-string-face "CornflowerBlue")
(setq font-lock-string-face 'font-lock-string-face)


;; function face
(copy-face             'bold 'font-lock-function-name-face)
(set-face-foreground         'font-lock-function-name-face "Red")
(set-face-background         'font-lock-function-name-face "Yellow")
(setq font-lock-function-name-face 'font-lock-function-name-face)

;; cond and other keywords face (IF ...)
(copy-face             'bold 'font-lock-keyword-face)
(set-face-foreground         'font-lock-keyword-face "ForestGreen")
(setq font-lock-keyword-face 'font-lock-keyword-face)

;; loop face 
(defvar font-lock-loop-face nil "Loops")
(copy-face             'bold 'font-lock-loop-face)
(set-face-foreground         'font-lock-loop-face "DarkViolet")
(set-face-background         'font-lock-loop-face "LemonChiffon")
(setq font-lock-loop-face 'font-lock-loop-face)

;; RUNTIME face 
(defvar font-lock-runtime-face nil "RUNTIME")
(copy-face             'bold 'font-lock-runtime-face)
(set-face-foreground         'font-lock-runtime-face "Black")
(set-face-background         'font-lock-runtime-face "Yellow")
(setq font-lock-runtime-face 'font-lock-runtime-face)


(defvar font-lock-andor-face nil "AndOr face")
(copy-face             'italic 'font-lock-andor-face)
(set-face-foreground         'font-lock-andor-face "Crimson")
(setq font-lock-andor-face 'font-lock-andor-face)

(defvar font-lock-declare-face nil "Declare face")
(copy-face             'bold 'font-lock-declare-face)
(set-face-foreground         'font-lock-declare-face "Blue")
(set-face-background         'font-lock-declare-face "Yellow")
(setq font-lock-declare-face 'font-lock-declare-face)

(defvar font-lock-system-face nil "System face")
(copy-face             'default 'font-lock-system-face)
(set-face-foreground         'font-lock-system-face "ForestGreen")
(setq font-lock-system-face 'font-lock-system-face)

(defvar font-lock-x-face nil "X face")
(copy-face             'bold 'font-lock-x-face)
(set-face-foreground         'font-lock-x-face "Yellow")
(set-face-background         'font-lock-x-face "Red")
(setq font-lock-x-face 'font-lock-x-face)

(defvar font-lock-false-face nil "False face")
(copy-face             'bold 'font-lock-false-face)
(set-face-foreground         'font-lock-false-face "Red")
(set-face-background         'font-lock-false-face "LightYellow")
(setq font-lock-false-face 'font-lock-false-face)

(defvar font-lock-true-face nil "True face")
(copy-face             'bold 'font-lock-true-face)
(set-face-foreground         'font-lock-true-face "Green")
(set-face-background         'font-lock-true-face "LightYellow")
(setq font-lock-true-face 'font-lock-true-face)

(defvar font-lock-y-face nil "Y face")
(copy-face             'bold 'font-lock-y-face)
(set-face-foreground         'font-lock-y-face "Red")
(setq font-lock-y-face 'font-lock-y-face)

(defvar font-lock-big-face nil "BIG face")
(copy-face             'default 'font-lock-big-face)
(set-face-foreground         'font-lock-big-face "DeepPink")
(setq font-lock-big-face 'font-lock-big-face)

(defvar font-lock-small-face nil "small face")
(copy-face             'default 'font-lock-small-face)
(set-face-foreground         'font-lock-small-face "red4")
(setq font-lock-small-face 'font-lock-small-face)

(defvar font-lock-number-face nil "number face")
(copy-face             'default 'font-lock-number-face)
(set-face-foreground         'font-lock-number-face "SpringGreen3")
(set-face-background         'font-lock-number-face "azure1")
(setq font-lock-number-face 'font-lock-number-face)

(defvar font-lock-dnumber-face nil "dnumber face")
(copy-face             'default 'font-lock-dnumber-face)
(set-face-foreground         'font-lock-dnumber-face "green4")
(set-face-background         'font-lock-dnumber-face "yellow")
(setq font-lock-dnumber-face 'font-lock-dnumber-face)

(defvar font-lock-z-face nil "Z face")
(copy-face             'bold 'font-lock-z-face)
(set-face-foreground         'font-lock-z-face "Blue")
(setq font-lock-z-face 'font-lock-z-face)

(defvar font-lock-fnutt-face nil "Fnutt face")
(copy-face             'bold 'font-lock-fnutt-face)
(set-face-foreground         'font-lock-fnutt-face "Red")
(setq font-lock-fnutt-face 'font-lock-fnutt-face)

(defvar font-lock-mystring-face nil "String face")
(copy-face             'default 'font-lock-mystring-face)
(set-face-foreground         'font-lock-mystring-face "CornflowerBlue")
(setq font-lock-mystring-face 'font-lock-mystring-face)

(defvar font-lock-ertstring-face nil "Ert String face")
(copy-face             'default 'font-lock-ertstring-face)
(set-face-foreground         'font-lock-ertstring-face "red")
(set-face-background         'font-lock-ertstring-face "SpringGreen1")
(setq font-lock-ertstring-face 'font-lock-ertstring-face)

(defvar font-lock-xertstring-face nil "X Ert String face")
(copy-face             'default 'font-lock-xertstring-face)
(set-face-foreground         'font-lock-xertstring-face "SpringGreen1")
(set-face-background         'font-lock-xertstring-face "red")
(setq font-lock-xertstring-face 'font-lock-xertstring-face)


(setq font-lock-string-face nil)


(defconst ipl-font-lock-keywords
  (list
   '("{.*?}" . font-lock-ertstring-face)
   '("@.*?@" . font-lock-xertstring-face)
   '("\\s //.*$" . font-lock-comment-face)
   '("^//.*$" . font-lock-comment-face)
;;   '("\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"" . font-lock-string-face)
   '("\".*?\"" . font-lock-mystring-face)
   '("(" . font-lock-y-face)
   '(")" . font-lock-y-face)
   '("\\[" . font-lock-z-face)
   '("\\]" . font-lock-z-face)
   '("\\b[0-9\\.]+e+[\\+\\-]*[0-9]+\\b" . font-lock-dnumber-face)  ;; 2.4e+22
   '("\\b[0-9\\.]*\\.+[0-9]+\\b" . font-lock-dnumber-face)
   '("\\.+[0-9]+\\b" . font-lock-dnumber-face)
   '("\\b[0-9]+\\b" . font-lock-number-face)
   '("\\b\\(DO\\|WHILE\\|ENDWHILE\\|ENDFOR\\|FOR\\|FROM\\|TO\\)\\b" . font-lock-loop-face)
   '("\\b\\(IF\\|THEN\\|ENDIF\\|ELSE\\)\\b" . font-lock-keyword-face)
   '("\\b\\(AND\\|OR\\|NOT\\)\\b" . font-lock-andor-face)
   '("\\b\\(FUNCTION\\|ENDFUNCTION\\|RETURN\\|HALT\\)\\b" . font-lock-function-name-face)
   '("\\b\\(Popup\\|EndPopup\\|Group\\|EndGroup\\)\\b" . font-lock-loop-face)
   '("\\b\\(RUNTIME\\|ENDRUNTIME\\)\\b" . font-lock-runtime-face)
   '("\\b\\(GLOBAL\\|String\\|Parameter\\|Int\\|Float\\|File\\|Zone\\|Bool\\|Points\\|Point\\|Polygons\\|GridModel\\)\\b" . font-lock-declare-face)
   '("\\b\\(Surface\\|Well\\|Log\\|File\\|Job\\|FunctionX\\|FunctionXY\\|DistributionX\\|DistributionXY\\)\\b" . font-lock-declare-face)
   '("\\b\\(FaciesModel\\|Body\\|BlockedLog\\)\\b" . font-lock-declare-face)

   '("\\b\\(Print\\|Label\\|WriteLine\\)\\b" . font-lock-z-face)
   '("@" . font-lock-x-face)
   '("FALSE" . font-lock-false-face)
   '("TRUE" . font-lock-true-face)
   '("\\<[a-z]+[a-z0-9_]*\\>" . font-lock-small-face)
   '("\\b[A-Z][A-Z0-9]+_*[A-Z0-9_]*\\b" . font-lock-big-face)             ;; global variables ala DEPTH_SURF
;;   '("\\b[A-Z][A-Z][A-Za-z]+\\b" . font-lock-system-face)
   '("\\<[A-Z]+[a-zA-Z0-9_]*\\>" . font-lock-system-face)
   '("\\<[a-z]+[A-Z0-9a-z_]+\\>" . font-lock-system-face)
   '("\\[" "\\]" . font-lock-system-face)
   '("\\s //.*$" . font-lock-comment-face)
   '("^//.*$" . font-lock-comment-face)
   )
"Expressions to highlight in RMS IPL mode.")



(provide 'ipl-mode)

(provide 'font-lock)

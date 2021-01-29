
;; This is basically a list of lists with a regular expression and
;; then the font name (or face name). The numbers below refer to \\(
;; ... \\) constructs that mark what part of the re you want
;; highlighted

;; JRIV made this febr. 2013, based on things made back in 2004

;; redefine or define face names
(defvar font-lock-ert-req-keyword-face nil "ERT required keyword")
(copy-face             'bold 'font-lock-ert-req-keyword-face)
(set-face-foreground         'font-lock-ert-req-keyword-face "Red")
(set-face-background         'font-lock-ert-req-keyword-face "Yellow")
(setq font-lock-ert-req-keyword-face 'font-lock-ert-req-keyword-face)

(defvar font-lock-ert-spec0-keyword-face nil "ERT special0 keywords")
(copy-face             'default 'font-lock-ert-spec0-keyword-face)
(set-face-foreground         'font-lock-ert-spec0-keyword-face "ForestGreen")
(set-face-background         'font-lock-ert-spec0-keyword-face "#7FFFD4")
(setq font-lock-ert-spec0-keyword-face 'font-lock-ert-spec0-keyword-face)

(defvar font-lock-ert-spec1-keyword-face nil "ERT special1 keywords")
(copy-face             'default 'font-lock-ert-spec1-keyword-face)
(set-face-foreground         'font-lock-ert-spec1-keyword-face "ForestGreen")
(set-face-background         'font-lock-ert-spec1-keyword-face "#FFDEAD")
(setq font-lock-ert-spec1-keyword-face 'font-lock-ert-spec1-keyword-face)

(defvar font-lock-ert-spec2-keyword-face nil "ERT special2 keywords")
(copy-face             'bold 'font-lock-ert-spec2-keyword-face)
(set-face-foreground         'font-lock-ert-spec2-keyword-face "Blue")
(set-face-background         'font-lock-ert-spec2-keyword-face "Khaki")
(setq font-lock-ert-spec2-keyword-face 'font-lock-ert-spec2-keyword-face)

(defvar font-lock-keyword-face nil "ERT tag")
(copy-face             'bold 'font-lock-keyword-face)
(set-face-foreground         'font-lock-keyword-face "Blue")
(setq font-lock-keyword-face 'font-lock-keyword-face)

(defvar font-lock-erttag-face nil "ERT tag")
(copy-face             'default 'font-lock-erttag-face)
(set-face-foreground         'font-lock-erttag-face "ForestGreen")
(setq font-lock-erttag-face 'font-lock-erttag-face)

(defvar font-lock-fnutt-face nil "Fnutt face")
(copy-face             'bold 'font-lock-fnutt-face)
(set-face-foreground         'font-lock-fnutt-face "Red")
(setq font-lock-fnutt-face 'font-lock-fnutt-face)

(defvar font-lock-upcase-face nil "Word UPPERCASE face")
(copy-face             'default 'font-lock-upcase-face)
(set-face-foreground         'font-lock-upcase-face "DarkViolet")
(setq font-lock-upcase-face 'font-lock-upcase-face)

(defvar font-lock-ertcomment-face nil "Comment")
(copy-face             'default 'font-lock-ertcomment-face)
(set-face-foreground         'font-lock-ertcomment-face "RosyBrown")
(setq font-lock-ertcomment-face 'font-lock-ertcomment-face)

(defvar font-lock-number-face nil "number face")
(copy-face             'default 'font-lock-number-face)
(set-face-foreground         'font-lock-number-face "black")
(set-face-background         'font-lock-number-face "PeachPuff")
(setq font-lock-number-face 'font-lock-number-face)

(defvar font-lock-dnumber-face nil "dnumber face")
(copy-face             'default 'font-lock-dnumber-face)
(set-face-foreground         'font-lock-dnumber-face "green4")
(set-face-background         'font-lock-dnumber-face "yellow")
(setq font-lock-dnumber-face 'font-lock-dnumber-face)

(defvar font-lock-ertstring-face nil "Ert String face")
(copy-face             'default 'font-lock-ertstring-face)
(set-face-foreground         'font-lock-ertstring-face "darkcyan")
(setq font-lock-ertstring-face 'font-lock-ertstring-face)


(setq font-lock-string-face nil)



(defconst ert-font-lock-keywords
  (list
   '("^--.*$" . font-lock-ertcomment-face)
   '("[[:space:]]+--.*$" . font-lock-ertcomment-face)
   ;;'("--.+\".+\"" . font-lock-ertcomment-face)
   '("\".+?\"" . font-lock-ertstring-face)
   ;; '("^--+.*\"+$" . font-lock-ertcomment-face)
   ;; '("[ ]*--+.*$" . font-lock-ertcomment-face)
   ;; '("\\s --.*$" . font-lock-ertcomment-face)
   '("\\b\\(^DATA_FILE\\|^ECLBASE\\|^JOBNAME\\|^INIT_SECTION\\|^NUM_REALIZATIONS\\^SCHEDULE_FILE\\)\\b" . font-lock-ert-req-keyword-face)
   ;; other keywords: all starts in first column and have large letters
   '("\\(<RUNPATH>\\)" . font-lock-ert-spec0-keyword-face)
   '("\\(<CONFIG_PATH>\\)" . font-lock-ert-spec1-keyword-face)
   ;;'("\\([^_]RUNPATH[^_]\\|CONFIG_PATH\\|ENSPATH\\)" . font-lock-ert-spec2-keyword-face)
   ;; '("^[^_][A-Z]+[_A-Z0-9]*" . font-lock-keyword-face)
   ;;'("[:space:]+[A-Z]+[_A-Z0-9]*[:space:]*" . font-lock-keyword-face)
   ;;'("\\b[A-Z][A-Z0-9]+_*[A-Z0-9_]*\\b" . font-lock-keyword-face)
   '("/" . font-lock-fnutt-face)
   '("=" . font-lock-fnutt-face)
   '("<[A-Z][_A-Z0-9]*>" . font-lock-erttag-face)
   '("^\\b[A-Z][A-Z0-9_:\\-]+_*[A-Z0-9_:\\-]*\\b" . font-lock-upcase-face)
   '("^\\b[A-Z]+[A-Z_]+[A-Z]\\b" . font-lock-upcase-face)
   '("^\\b[A-Z]+[A-Z_]+[0-9]\\b" . font-lock-upcase-face)
   '("[[:space:]]+[A-Z]+[_A-Z0-9]+" . font-lock-keyword-face)
   ;;'("\\b[0-9\\.]+e+[\\+\\-]*[0-9]+\\b" . font-lock-dnumber-face)  ;; 2.4e+22
   ;;'("\\b[0-9\\.]*\\.+[0-9]+\\b" . font-lock-dnumber-face)
   ;;'("\\.+[0-9]+\\b" . font-lock-dnumber-face)
   ;;'("\\b[0-9]+\\b" . font-lock-number-face)

  )
"Expressions to highlight in ERT mode.")


(defun ert-mode ()
  "Major mode for ERT"
  (interactive)
  (kill-all-local-variables)
  (setq font-lock-defaults '(ert-font-lock-keywords))
  (setq major-mode 'ert-mode)
  (setq mode-name "ERT Mode by JRIV - Statoil")
  (font-lock-mode 1)
  (make-local-variable 'comment-start)
  (setq comment-start "-- ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
)

(provide 'ert-mode)

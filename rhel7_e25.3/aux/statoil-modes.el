;;============================================================================
;; Purpose  : Generic mode to ble loaded from .emacs file
;;
;; Usage    : Use (load "<path>/statoil-modes") in .emacs
;;
;; Comments :
;; Creator  : Jan C. Rivenaes
;; Invented : 05-DEC-96 JRIV (original version)
;;
;; $Id: statoil-modes.el 466 2017-02-15 06:24:30Z jriv $
;;============================================================================
;;
(global-font-lock-mode t)

;; Default location of mode files:
(setq load-path (append load-path (list "~jriv/.emacs.d/aux")))
;; (setq load-path (append load-path (list "/project/res/etc/emacs-modes")))


;; IRMS IPL mode
;;
(autoload 'ipl-mode "ipl-mode" "RMS IPL" t)
(setq auto-mode-alist (cons '("\\.irms" . ipl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ipl" . ipl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.iplib" . ipl-mode) auto-mode-alist))
;;
;; Eclipse mode
;;
(autoload 'eclipse-mode "eclipse" "ECLIPSE" t)
(setq auto-mode-alist (cons '("\\.DATA" .   eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.data" .   eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.GRDECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.grdecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.EDTECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.edtecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.PRPECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.prpecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.SUMECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sumecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.SCHECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.schecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.SCH" .    eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sch" .    eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.SOLECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.solecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.REGECL" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.regecl" . eclipse-mode) auto-mode-alist))

;; ERT config file mode
;;
(autoload 'ert-mode "ert-mode" "ERT" t)
(setq auto-mode-alist (cons '("\\.ert" .   ert-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ertcfg" .   ert-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ERT" .   ert-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ERTCFG" .   ert-mode) auto-mode-alist))

;; YAML mode
;; (not made by Statoil, but not in standards emacs)
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(setq auto-mode-alist (cons '("\\.yaml" .   yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml" .   yaml-mode) auto-mode-alist))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

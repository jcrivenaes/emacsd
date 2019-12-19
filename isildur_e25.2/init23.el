;; My own stuff ---------------------------------------------------------------
;;(add-to-list 'load-path "~/emacs/")

;;(add-to-list 'custom-theme-load-path "~/emacs-modes")

(load "/private/jriv/emacs-modes/highlight-chars")
;; use one of these:

(load "/project/res/etc/emacs-modes/statoil-modes")
;;(load "/private/jriv/work/svn/emacs-modes/statoil-modes-test")

;;(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "#FFF7E5"))

;; highlight selection
(transient-mark-mode 1)

;; comment/uncomment region
(global-set-key '[(f1)]          'comment-dwim)

;; comment/uncomment region
(global-set-key '[(f2)]          'column-number-mode)

;; comment/uncomment region
(global-set-key '[(f3)]          'whitespace-mode)


(global-set-key '[(f5)]          'hc-toggle-highlight-tabs)
(global-set-key '[(f6)]          'hc-toggle-highlight-trailing-whitespace)

;; More of my own stuff -------------------------------------------------------

;; ----------------------------------------------------------YAML--------------
;;(load "/private/jriv/emacs-modes/yaml-mode")

;;(require 'yaml-mode)
;;    (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;;(add-hook 'yaml-mode-hook
;;	  '(lambda ()
;;	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;; ----------------------------------------------------------PERL--------------


(defalias 'perl-mode 'cperl-mode)

(setq cperl-indent-level 4)
(setq cperl-invalid-face 'default) ;; remove the left line after tab/space

(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))

(setq auto-mode-alist (cons '("\\.t\\'" . perl-mode) auto-mode-alist))

;; ----------------------------------------------------------C-----------------

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Change the indentation amount to 4 spaces instead of 2.
;; You have to do it in this complicated way because of the
;; strange way the cc-mode initializes the value of `c-basic-offset'.
;;(add-hook 'c-mode-hook (lambda () (setq c-basic-offset 4)))
(setq-default c-basic-offset 4)


;; ----------------------------------------------------------Python------------

;;(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
;;(add-hook 'python-mode-hook (lambda ()
;;                              (guess-style-guess-tab-width)))

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

(add-hook 'python-mode-hook
          (lambda () 
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Set tab to display as 4 spaces
;;(setq-default tab-width 4)
;; Set stop-tabs to be 4 written as spaces
;;(setq-default tab-stop-list (number-sequence 4 120 4))

;; Set default tab space for various modes
;;(setq-default sgml-basic-offset 4)
;;(setq-default py-indent-offset 4)
;;(setq-default python-indent 4)

;; highlight tabs and trailing whitespace
;; (require 'highlight-chars)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
;; (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

;; ----------------------------------------------------------aux---------------
;; backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.



;; break long lines according to window width...
(autoload 'longlines-mode
   "longlines.el"
   "Minor mode for automatically wrapping long lines." t)

;;(setq longlines-wrap-follows-window-size t)

;;(setq fill-column 78)
;;(setq auto-fill-mode t)

(defun remove-hard-wrap-paragraph () 
  "Replace newline chars in current paragraph by single spaces."
  (interactive) 
  (let ((fill-column 90002000)) 
    (fill-paragraph nil)))

(defun remove-hard-wrap-region (start end)
  "Replace newline chars in region by single spaces." 
  (interactive "r")
  (let ((fill-column 90002000)) 
    (fill-region start end)))



;; My own stuff end here ------------------------------------------------------


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(perl-indent-continued-arguments 5))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;;(add-to-list 'default-frame-alist
;;            '(font . "Courier-12"))


(defun print-to-pdf ()
 (interactive)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "ps2pdf14 tmp.ps " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (message (concat "File printed in : "(buffer-name) ".pdf"))
 )

;; for this command: emacs $File --eval "(print-to-pdf-batch)"
(defun print-to-pdf-batch ()
 (interactive)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "ps2pdf14 tmp.ps " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (kill-emacs t)
 )

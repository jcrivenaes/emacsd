;;======================================================================================
;; INSTALL PACKAGES
;;======================================================================================

(require 'package)
(message "Initialize init25.el")
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/") t)

;; activate all packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; define list of packages to install
(defvar myPackages
  '(better-defaults
    exec-path-from-shell
    elpy
    pyenv-mode
    theme-looper
    ample-theme
    dakrone-theme
    danneskjold-theme
    darcula-theme
    material-theme
    column-enforce-mode
    flycheck
    minimap
    projectile
    projectile-speedbar
    pylint
    sublimity))

;; install all packages in list
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;;======================================================================================
;; BASIC
;;======================================================================================

;; backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Use shell's $PATH
;; (exec-path-from-shell-copy-env "PATH")

(global-linum-mode t)              ;; enable line numbers globally
(setq linum-format "%4d \u2502 ")  ;; format line number spacing

;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; remove trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; column enforce
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'ipl-mode-hook 'column-enforce-mode)
(setq column-enforce-column 88)

;; some settings
(setq load-prefer-newer t)

(add-to-list 'load-path "~/.emacs.d/aux")

;; backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;;======================================================================================
;; Sublimity
;;======================================================================================
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)
(setq sublimity-map-size 20)
(setq sublimity-map-fraction 0.3)
(setq sublimity-map-text-scale -7)
(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))
;;======================================================================================
;; F keys
;;======================================================================================

(global-set-key '[(f1)]          'comment-dwim)

(global-set-key '[(f2)]          'column-number-mode)

(global-set-key '[(f3)]          'whitespace-mode)

(global-set-key '[(f4)]          'find-file-at-point)

(global-set-key '[(f5)]          'hc-toggle-highlight-tabs)
(global-set-key '[(f6)]          'hc-toggle-highlight-trailing-whitespace)

(global-set-key [f7]             'minimap-mode)

(global-set-key '[(f8)]          'undo)

(global-set-key '[(f9)]          'elpy-black-fix-code)
(global-set-key '[(ctrl f9)]     'pylint)
(global-set-key '[(shift f9)]    'minimap-mode)

(global-set-key '[(f10)]         'column-enforce-mode)
(global-set-key '[(f11)]         'theme-looper-enable-next-theme)
(global-set-key '[(f12)]         'text-scale-increase)
(global-set-key '[(shift f12)]   'text-scale-decrease)


;;======================================================================================
;; THEMES
;;======================================================================================

(setq inhibit-startup-message t)   ;; hide the startup message
(load-theme 'material t)           ;; load material theme
(load-theme 'ample t t)           ;; load material theme
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(load-theme 'danneskjold t t)
(load-theme 'darcula t t)
(enable-theme 'ample)

;;======================================================================================
;; ELPY and PYTHON
;;======================================================================================

(elpy-enable)
(setq elpy-rpc-backend "rope")

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(pyenv-mode)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

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


;; remove trailing whitespace when saving sdsd
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;======================================================================================
;; C code
;;======================================================================================
;; Change the indentation amount to 4 spaces instead of 2.
(setq-default c-basic-offset 4)
(setq c-recognize-knr-p nil)

;; ;;======================================================================================
;; ;; CUSTOM SET aka Don't FUZZ
;; ;;======================================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" default)))
 '(package-selected-packages
   (quote
    (sublimity pylint projectile-speedbar projectile minimap flycheck column-enforce-mode material-theme darcula-theme danneskjold-theme dakrone-theme ample-theme theme-looper pyenv-mode elpy exec-path-from-shell better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

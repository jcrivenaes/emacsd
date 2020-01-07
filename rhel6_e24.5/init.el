;;======================================================================================
;; INSTALL PACKAGES
;;======================================================================================
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
	("http" . "www-proxy.statoil.no:80")
	("https" . "www-proxy.statoil.no:80")))

(require 'package)
(message "Initialize init.el")
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/aux")

(message "..step1")

;; activate all packages
(package-initialize)

(message "..step2")
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(message "..step3")

;; define list of packages to install
(defvar myPackages
  '(elpy
    theme-looper
    darcula-theme
    badwolf-theme
    ample-theme
    cmake-mode
    material-theme
    column-enforce-mode
    flycheck
    el-get
    company
    yaml-mode
    flycheck-yamllint
    smartparens
    toml-mode
    pylint))

(message "..step4")
;; install all packages in list
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(message "..step5")

;;======================================================================================
;; BASIC
;;======================================================================================
(message "BASIC...")

(when (string= (getenv "USER") "jriv")
  ;; use one of these:
  (load "/project/res/etc/emacs-modes/statoil-modes")
  ;;(load "/private/jriv/work/svn/emacs-modes/statoil-modes-test")
)

;; Set default font
(set-face-attribute 'default nil
                    :height 140
                    :weight 'normal
                    :width 'normal)

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

;; backup
(setq backup-directory-alist `(("." . "~/.saves")))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; smartparents
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
;;======================================================================================
;; F keys
;;======================================================================================
(message "F keys")

(global-set-key '[(f1)]          'comment-dwim)

(global-set-key '[(f2)]          'column-number-mode)

(global-set-key '[(f3)]          'whitespace-mode)

(global-set-key '[(f4)]          'find-file-at-point)
(global-set-key '[(ctrl f4)]     'shift-right)
(global-set-key '[(shift f4)]    'shift-left)

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
(message "THEMES....")

(setq inhibit-startup-message t)   ;; hide the startup message
(load-theme 'material t)           ;; load material theme
(load-theme 'darcula t t)
(load-theme 'ample t t)
(enable-theme 'ample)

;;======================================================================================
;; ELPY and PYTHON
;;======================================================================================
(message "ELPY etc...")

(elpy-enable)
(setq elpy-rpc-backend "rope")
(setq elpy-rpc-virtualenv-path 'current)

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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
(message "C code...")
(setq c-default-style "stroustrup")

;; Windows performance tweaks for irony
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-hook 'company-mode)

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;;======================================================================================
;; LOCAL CUSTOM SET shall not be part .git
;;======================================================================================

(message "Local stuff")
(setq custom-file "~/.emacs.d/init_local.el")

(load custom-file)

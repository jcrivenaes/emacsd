

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (string= (getenv "USER") "jriv")
  (message "USER is JRIV, resolve proxy...")
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "www-proxy.statoil.no:80")
          ("https" . "www-proxy.statoil.no:80"))))

(cond ((< emacs-major-version 24)
       (message "version 23 or earlier")
       (setq custom-file "~/.emacs.d/init23.el")))

(cond ((= emacs-major-version 24)
       (message "version 24")
       (setq custom-file "~/.emacs.d/init24.el")))

(cond ((= emacs-major-version 25)
       (message "version 25")
       (setq custom-file "~/.emacs.d/init25.el")))

(cond ((= emacs-major-version 26)
       (message "version 26 (use 25)")
       (setq custom-file "~/.emacs.d/init25.el")))

(load custom-file)

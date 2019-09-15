(package-initialize)

(when (string= (getenv "USER") "jriv")
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

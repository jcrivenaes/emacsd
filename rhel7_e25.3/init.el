;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(cond ((= emacs-major-version 24)
       (message "version 24")
       (setq custom-file "~/.emacs.d/init24.el")))

(cond ((= emacs-major-version 27)
       (message "version 27")
       (setq custom-file "~/.emacs.d/init27.el")))

(load custom-file)

;;; theme-looper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "theme-looper" "theme-looper.el" (23906 57727
;;;;;;  382978 494000))
;;; Generated autoloads from theme-looper.el

(autoload 'theme-looper-set-favorite-themes "theme-looper" "\
Sets the list of color-themes to cycle thru

\(fn THEMES)" nil nil)

(autoload 'theme-looper-set-favorite-themes-regexp "theme-looper" "\
Sets the list of color-themes to cycle thru, matching a regular expression

\(fn REGEXP)" nil nil)

(autoload 'theme-looper-set-ignored-themes "theme-looper" "\
Sets the list of color-themes to ignore

\(fn THEMES)" nil nil)

(autoload 'theme-looper-set-ignored-themes-regexp "theme-looper" "\
Sets the list of color-themes to ignore, matching a regular expression

\(fn REGEXP)" nil nil)

(autoload 'theme-looper-set-post-switch-script "theme-looper" "\
Sets script to be run after every theme switch

\(fn FUNC)" nil nil)

(autoload 'theme-looper-reset-themes-selection "theme-looper" "\
Resets themes selection back to default

\(fn)" nil nil)

(autoload 'theme-looper-enable-theme "theme-looper" "\
Enables the specified color-theme

\(fn THEME)" nil nil)

(autoload 'theme-looper-enable-next-theme "theme-looper" "\
Enables the next color-theme in the list

\(fn)" t nil)

(autoload 'theme-looper-enable-previous-theme "theme-looper" "\
Enables the previous color-theme in the list

\(fn)" t nil)

(autoload 'theme-looper-enable-random-theme "theme-looper" "\
Enables a random theme from the list

\(fn)" t nil)

(autoload 'theme-looper-select-theme "theme-looper" "\
Lets user select a theme from a list of favorite ones rendered using ivy API

\(fn)" t nil)

(autoload 'theme-looper-select-theme-from-all "theme-looper" "\
Lets user select a theme from a list of all available themes rendered using ivy API

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; theme-looper-autoloads.el ends here

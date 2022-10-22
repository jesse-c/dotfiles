(setq package-enable-at-startup nil)

;; Don't resize the frame to preserve the number of columns or lines
;; being displayed when setting font, menu bar, tool bar, tab bar,
;; internal borders, fringes, or scroll bars.  Since I use XMonad, this
;; option is i) useless anyways and ii) _terribly_ expensive.
(setq frame-inhibit-implied-resize t)

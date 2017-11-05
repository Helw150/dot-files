;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(package-initialize)  ;load and activate packages, including auto-complete

;; Autocomplete active
(ac-config-default)
(global-auto-complete-mode t)


;; General Emacs Settings
(setq c-basic-offset 4) ; indents 4 chars
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces
(setq inhibit-startup-screen t)

;; Highlight current lines
(global-hl-line-mode t)
(hlinum-activate)

;; Highlight matching parens
(show-paren-mode 1)

;; I'm using CLI 99% of the time - Get the menu bar out of my face
(menu-bar-mode -1)

;; Always show line numbers
(global-linum-mode t)

;; 4 Digits accounted for with Left Justification
(setq linum-format "%-4d\u2502 ")

;; Wrap text
(global-visual-line-mode 1)

;; Highlights the present line with a very faint glow
(set-face-attribute 'hl-line nil :background "#4C4C4C")
(set-face-attribute 'linum-highlight-face nil :weight 'bold :background "#4C4C4C" :foreground "#9FC59F")
(set-face-attribute 'linum nil :foreground "#537953")

;; Flycheck
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;; Ivy
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Use Web-Mode JSX for all js or jsx files
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; Setting up Zenburn Theme
(add-hook 'after-init-hook (lambda () (load-theme 'zenburn t)))

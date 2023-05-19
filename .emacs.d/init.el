;; I'm using CLI 99% of the time - Get the menu bar out of my face
(menu-bar-mode -1)
(require 'package)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-list '(zenburn-theme company yasnippet ivy swiper counsel multiple-cursors key-chord expand-region projectile magit ace-jump-mode reason-mode markdown-mode python-black use-package))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))

;; General Emacs Settings
(setq c-basic-offset 4) ; indents 4 chars
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces
(setq inhibit-startup-screen t)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

(use-package python-black
  :demand t
  :hook
  (python-mode . python-black-on-save-mode)
  :after python)

;; JUMP AROUND
(key-chord-define-global "we"     'ace-jump-mode)

;; Project Management with Projectile
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching t)

;; Macro Keybindings
(key-chord-define-global ">>"     'forward-word)
(key-chord-define-global "<<"     'backward-word)


;; Multi-Cursor Using Key Chord
(key-chord-mode +1)
(key-chord-define-global "es"     'mc/edit-lines)
(key-chord-define-global "e]"     'mc/mark-next-like-this)
(key-chord-define-global "e["     'mc/mark-previous-like-this)
(key-chord-define-global "ea"     'mc/mark-all-like-this)

;; Expand region key chord
(key-chord-define-global "``" 'er/expand-region)

;; Highlight matching parens
(show-paren-mode 1)

;; Wrap text
(global-visual-line-mode 1)

;; Highlights the present line with a very faint glow
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
(set-face-background 'hl-line "#4C4C4C")

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

;; Use Scala mode for .sc files
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; Use Scala mode for .sbt files
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

;; Setting up Zenburn Theme
(add-hook 'after-init-hook (lambda () (load-theme 'zenburn t)))

;; Yasnippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/indent-line nil)
(yas-global-mode 1)

;; Company Abbreviation in the window using dabbrev and Yasnippets search with company
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-dabbrev-code))
  (setq company-tooltip-limit 20)                      ; bigger popup window
  (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (global-set-key (kbd "M-/") 'company-yasnippet)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (reason-mode ace-jump-mode magit zenburn-theme xclip web-mode py-yapf projectile prettier-js multiple-cursors key-chord expand-region ensime counsel add-node-modules-path))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

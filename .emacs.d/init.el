(setq package-list '(zenburn-theme company yasnippet ivy swiper counsel hlinum web-mode xclip py-yapf prettier-js add-node-modules-path ensime scala-mode sbt-mode multiple-cursors key-chord))
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(package-initialize)  ;load and activate packages, including auto-complete

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

;; Multi-Cursor Using Key Chord
(key-chord-mode +1)
(key-chord-define-global "ss"     'mc/edit-lines)


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

;; Beautify Python on save
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;; Wrap text
(global-visual-line-mode 1)

;; Highlights the present line with a very faint glow
(set-face-attribute 'hl-line nil :background "#4C4C4C")
(set-face-attribute 'linum-highlight-face nil :weight 'bold :background "#4C4C4C" :foreground "#9FC59F")
(set-face-attribute 'linum nil :foreground "#537953")

;; Sync terminal and system clipboard
(xclip-mode 1)

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)
(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'add-node-modules-path)
          (add-hook 'web-mode-hook #'prettier-js-mode)))   

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


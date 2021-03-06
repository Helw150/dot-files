;; I'm using CLI 99% of the time - Get the menu bar out of my face
(menu-bar-mode -1)

(setq package-list '(zenburn-theme company yasnippet ivy swiper counsel hlinum web-mode prettier-js add-node-modules-path ensime scala-mode sbt-mode multiple-cursors key-chord expand-region projectile magit ace-jump-mode reason-mode markdown-mode gh-md))
;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;;'("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
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
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

;; Reason Mode Auto Prettify
(add-hook 'reason-mode-hook (lambda ()
          (add-hook 'before-save-hook 'refmt-before-save)))

;; JUMP AROUND
(key-chord-define-global "we"     'ace-jump-mode)

;; Project Management with Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

;; Highlight current lines
(global-hl-line-mode t)
(hlinum-activate)

;; Highlight matching parens
(show-paren-mode 1)


;; Always show line numbers
(global-linum-mode t)

;; 4 Digits accounted for with Left Justification
(setq linum-format "%-4d\u2502 ")

;; Beautify Python on save
(defcustom python-yapf-path (executable-find "yapf")
  "yapf executable path."
  :group 'python
  :type 'string)

(defun python-yapf ()
    "Automatically formats Python code to conform to the PEP 8 style guide.
$ yapf --in-place <filename>"
    (interactive)
    (when (eq major-mode 'python-mode)
      (shell-command
       (format "%s --in-place %s" python-yapf-path
               (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t)))

(eval-after-load 'python
  '(if python-yapf-path
              (add-hook 'after-save-hook 'python-yapf)))


(defun render-md ()
    "Renders Markdown in a Window Parallel to the Markdown using the Github API"
    (interactive)
    (when (eq major-mode 'markdown-mode)
      (gh-md-render-buffer)))

(eval-after-load 'markdown
              (add-hook 'after-save-hook 'render-md))

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

;; OSX Copy Paste
(osx-clipboard-mode +1)

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
    (osx-clipboard reason-mode ace-jump-mode magit zenburn-theme xclip web-mode py-yapf projectile prettier-js multiple-cursors key-chord hlinum expand-region ensime counsel add-node-modules-path))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

;;; init.el --- sane, fast, CLI-first Emacs

;; UI: keep it minimal
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; Packages --------------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 15)
        ("gnu"          . 10)
        ("melpa"        . 5)))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install what we actually use
(setq package-list
      '(use-package
        zenburn-theme
        ivy swiper counsel
        projectile magit
        company yasnippet
        multiple-cursors key-chord expand-region
        ace-jump-mode
        markdown-mode gh-md
        ruff-format
        flycheck
        scala-mode
        reason-mode))

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (ignore-errors (package-install pkg))))

;; Editing basics --------------------------------------------------------------
(setq c-basic-offset 4
      tab-width 4
      indent-tabs-mode nil)

(show-paren-mode 1)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#4C4C4C")

;; Modern line numbers (replace old linum)
(global-display-line-numbers-mode t)
(setq display-line-numbers-width 4)

;; Theme
(add-hook 'after-init-hook (lambda () (load-theme 'zenburn t)))

;; Ivy/Swiper/Counsel -----------------------------------------------------------
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

;; Projectile
(projectile-mode +1)
(setq projectile-completion-system 'ivy
      projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Company + Yasnippet ----------------------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends '(company-dabbrev-code))
  (setq company-tooltip-limit 20
        company-idle-delay 0.3)
  (global-set-key (kbd "M-/") 'company-yasnippet))
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Key-chord / multicursor / expand-region -------------------------------------
(require 'key-chord)
(key-chord-mode +1)

;; Jump
(key-chord-define-global "we" 'ace-jump-mode)

;; Word “macros”
(key-chord-define-global ">>" 'forward-word)
(key-chord-define-global "<<" 'backward-word)

;; Multicursor
(key-chord-define-global "es" 'mc/edit-lines)
(key-chord-define-global "e]" 'mc/mark-next-like-this)
(key-chord-define-global "e[" 'mc/mark-previous-like-this)
(key-chord-define-global "ea" 'mc/mark-all-like-this)

;; Expand region
(key-chord-define-global "``" 'er/expand-region)

;; Soft wrap
(global-visual-line-mode 1)

;; Python formatting (pick one; we keep Ruff) -----------------------------------
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'ruff-format-on-save-mode))

;; Flycheck (guarded so it won’t error if missing) ------------------------------
(unless (package-installed-p 'flycheck)
  (ignore-errors (package-install 'flycheck)))

(with-eval-after-load 'flycheck
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-gcc-language-standard "c++11"))))

(when (locate-library "flycheck")
  (autoload 'global-flycheck-mode "flycheck" nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Markdown: render via GitHub API on save (gh-md) ------------------------------
(defun render-md ()
  "Render Markdown buffer via GitHub API in a parallel window."
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (gh-md-render-buffer)))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'render-md nil t))))

;; Scala associations -----------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.sc\\'"   . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'". scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'"  . scala-mode))

;; Ubuntu / TTY / SSH clipboard helpers ----------------------------------------
(defun copy-from-ubuntu ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to X clipboard.")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard."))
      (message "No region active; can't yank."))))

(defun cut-from-ubuntu ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to X clipboard.")
        (call-interactively 'clipboard-kill-ring-save))
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (delete-region (region-beginning) (region-end))
          (message "Yanked region to clipboard."))
      (message "No region active; can't yank."))))

(defun paste-to-ubuntu ()
  (interactive)
  (if (display-graphic-p)
      (progn (clipboard-yank) (message "graphics active"))
    (insert (shell-command-to-string "xsel -o -b"))))

;; OSC52: copy last kill via TTY/tmux/SSH
(defun yank-to-clipboard ()
  "Use ANSI OSC 52 escape sequence to attempt clipboard copy."
  (interactive)
  (let* ((text (or (car kill-ring) ""))
         (base64_text (base64-encode-string (encode-coding-string text 'utf-8) t))
         (tmx_tty (string-trim
                   (shell-command-to-string
                    "ps l | grep 'tmux attach' | grep -v grep | awk '{print $11}'"))))
    (cond
     ((getenv "TMUX")
      (shell-command (format "printf \"\\033]52;c;%s\\a\" > /dev/%s" base64_text tmx_tty)))
     ((getenv "SSH_TTY")
      (shell-command (format "printf \"\\033]52;c;%s\\a\" > %s" base64_text (getenv "SSH_TTY"))))
     (t
      (send-string-to-terminal (format "\033]52;c;%s\a" base64_text))))))

;; Customization blocks ---------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(reason-mode ace-jump-mode magit zenburn-theme projectile multiple-cursors key-chord expand-region counsel scala-mode gh-md ruff-format flycheck ivy swiper company yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; init.el ends here

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.
Return a list of installed packages or nil for every package not installed."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))
 
(require 'package)
;; Marmalade package repository
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(package-initialize)
 
;;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)
 
;;; kill&yank from X11
(setq x-select-enable-clipboard t
       x-select-enable-primary t
       save-interprogram-paste-before-kill t
       apropos-do-all t
       mouse-yank-at-point t)
 
;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
 
;;;; company-mode
(ensure-package-installed 'company)
(add-hook 'after-init-hook 'global-company-mode)
 
;;;; paredit
(ensure-package-installed 'paredit)
(require 'paredit)
 
;;;; Autopair
(ensure-package-installed 'autopair)
(require 'autopair)
 
(defun add-hooks-to-mode (mode-hook functions)
  (dolist (hook functions)
    (add-hook mode-hook hook)))
 
;;;; elisp
(add-hooks-to-mode 'emacs-lisp-mode-hook
                   '(enable-paredit-mode
                     turn-on-eldoc-mode))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
 
;;;; Scheme
(ensure-package-installed 'geiser)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'geiser-mode)
(setq geiser-guile-load-init-file-p t)

;;;; Flycheck

(ensure-package-installed 'flycheck 'flycheck-haskell)
(require 'flycheck)
(require 'flycheck-haskell)
(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables
 '(flycheck-idle-check-delay 2))

;;;; Haskell

(ensure-package-installed 'haskell-mode)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(add-hooks-to-mode 'haskell-mode-hook
                   '(turn-on-haskell-indentation
                     turn-on-haskell-doc-mode))
(custom-set-variables
 '(haskell-process-suggest-remove-import t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-type 'ghci))
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
 
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(geiser-guile-jump-on-debug-p t)
 '(geiser-guile-load-init-file-p t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work- right.
 )

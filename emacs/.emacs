;;;; GENERAL

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every package not installed."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))

(defun add-hooks-to-mode (mode-hook functions)
  (dolist (hook functions)
    (add-hook mode-hook hook)))
 
(require 'package)
;; Marmalade package repository
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; MELPA package repository
;(add-to-list 'package-archives
;  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(setq package-enable-at-startup nil)
;(package-initialize)

;(ensure-package-installed 'melpa)
;; TODO write a function to make this local (argument to ensure-package-installed?)
;(setq package-archive-enable-alist '(("melpa" melpa powerline yasnippet haskell-mode python-environment geiser)))

(ensure-package-installed 'evil 'evil-leader)
(require 'evil)
(require 'evil-leader)

;;; Start emacs as a server
;(server-start)

;;;; LOOK
(ensure-package-installed 'solarized-theme)

(set-face-attribute 'default nil :font "Inconsolata" :height 130)
; server font bug
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
(blink-cursor-mode 0)
(load-theme 'solarized-dark t)
;(ensure-package-installed 'powerline)
;(require 'powerline)
;(powerline-center-evil-theme)

;;; buffers with the same name get folder prefixed
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; Disable GUI
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))



;;;; FEEL
(setq x-super-keysym 'meta)
(global-set-key (kbd "RET") 'newline-and-indent) ;; Return should indent
(setq undo-limit 10000) ;; Some more undo steps
(global-undo-tree-mode)
(defalias 'yes-or-no-p 'y-or-n-p) ;; No typing yes please
(setq-default indent-tabs-mode nil) ;; Death to non-smart tabs
(setq-default fill-column 80)

;;;; Save on focus lost in X11
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;; jump to the line where you left
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;;; more powerful commands
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; kill&yank from X11
(setq x-select-enable-clipboard t
       x-select-enable-primary t
       save-interprogram-paste-before-kill t
       apropos-do-all t
       mouse-yank-at-point t)

;;; make backups in this folder
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))



;;;; MODES


;;;; Info
(add-hook 'Info-mode-hook 'turn-off-evil-mode)


;;;; eldoc
(eldoc-mode 1) ; enable globally

;;;; company-mode
(ensure-package-installed 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;; auto-complete (replaced by company-mode
;; (ensure-package-installed 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (custom-set-variables
;;  '(ac-auto-show-menu t)
;;  '(ac-auto-start 4)
;;  '(ac-quick-help-delay 0.3)
;;  '(ac-use-fuzzy nil)
;;  ;'(ac-max-width 0.5)
;;  )
;; (setq ac-auto-show-menu t) ;; Don’t show the menu automatically
;; (setq ac-auto-start 4)       ;; Autocomplete after 4 letters
;; (setq ac-quick-help-delay 0.3) ;; Show the help faster
;; (setq ac-use-fuzzy nil) ;; fuzzy matching!
;; (setq ac)
;; (define-key ac-mode-map (kbd "TAB") 'auto-complete)

;;;; paredit
(ensure-package-installed 'paredit)
(require 'paredit)
(define-key paredit-mode-map
  (kbd "C-M-q") 'paredit-reindent-defun)

(evil-define-key 'normal paredit-mode-map
  "D" 'paredit-kill
  "W" 'paredit-forward
  "B" 'paredit-backward
  ">" 'paredit-forward-slurp-sexp
  "<" 'paredit-forward-barf-sexp
  )

(evil-leader/set-key-for-mode 'paredit-mode
  "w" 'paredit-wrap-round
  "s" 'paredit-split-sexp
  "r" 'paredit-raise-sexp
  "j" 'paredit-join-sexps)

(evil-define-key 'insert paredit-mode-map
  (kbd "C-w") 'paredit-backward-kill-word
  (kbd "C-e") 'paredit-forward-slurp-sexp)


;;;; Comint (REPLs)
(require 'comint)
(define-key comint-mode-map (kbd "C-n") 'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "C-p") 'comint-previous-matching-input-from-input)


;;;; Edebug
(add-hook 'edebug-mode-hook 'turn-off-evil-mode)
(define-key emacs-lisp-mode-map (kbd "C-M-1") 'edebug-defun)


;;;; PLUGINS

(add-to-list 'load-path "~/.emacs.d/scripts")

;;;; ido
(ensure-package-installed 'ido 'smex)

(ido-mode t)
(setq ido-enable-flex-matching t)

;;; shadows find-file with this dispatch-lambda
(setq native-find-file-func (symbol-function 'find-file)) ;; original find-file code
(fset 'find-file (lambda (filename &optional wildcards)
                   (if filename
                       (funcall native-find-file-func filename wildcards)
                     (ido-find-file))))
(defalias 'switch-buffer 'ido-switch-buffer) ;; use better buffer switcher

;;;; imenu

(add-to-list 'load-path "~/.emacs.d/scripts/imenu-anywhere")
(require 'imenu-anywhere)

;;; smex
(require 'smex)
(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(smex-auto-update 60)
(setq smex-save-file "~/.emacs.d/smex-items")


;;;; Evil (Vim)
(global-evil-leader-mode)
(ensure-package-installed 'evil 'evil-leader 'surround 'evil-nerd-commenter)
(require 'evil-leader) ;; Needs execution order to work in every buffer …
(require 'evil)
; Normal State
(defun evil-empty-line-below ()
  (interactive)
  (evil-open-below 0)
  (evil-normal-state))
; Normal undo is way too coarse in evil
(setq evil-want-fine-undo t) ;;TODO: new version: "fine" option?
; Search for symbols instead of words
(setq-default evil-symbol-word-search t)

(defun describe-function-at-point-active-window ()
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (describe-function symb))
    (other-window 1)))

; can this be removed?
;; (define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
;; (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
(setq normal-state-mappings '(
    ("RET" . evil-empty-line-below)
    ("K"   . describe-function-at-point-active-window)
    ("]d"  . find-function-at-point)
    ("TAB" . evil-undefine) ;; Don’t block <tab>, damnit!
    ("SPC" . evil-toggle-fold)
    ("M-." . find-tag)
    ))

(let ((keys normal-state-mappings))
  (dolist (key keys)
    (define-key evil-normal-state-map
      (kbd (car key))
      (cdr key))))

; Neo2-evil

(evil-mode 1)

;;;; evil-surround
(require 'surround)
(global-surround-mode 1)

;;;; evil-leader
(require 'evil-leader)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader ",")
(evil-mode nil) ;; no idea
(evil-mode 1)
(evil-leader/set-key
  "a" 'evil-window-down
  "l" 'evil-window-up
  "i" 'evil-window-left
  "e" 'evil-window-right
  "b" 'switch-buffer
  "f" 'find-file)

;;;; evil-nerd-commenter
(require 'evil-nerd-commenter)
(global-set-key (kbd "C-c SPC") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
(evil-leader/set-key
  "c SPC" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region)

;;;; undo-tree-mode
(add-hook 'undo-tree-visualizer-hook 'turn-off-evil-mode)

;;;; scpaste
(ensure-package-installed 'scpaste)

(require 'scpaste)
(setq scpaste-http-destination "https://bigmac.caelum.uberspace.de/paste"
      scpaste-scp-destination "bigmac@uberspace:html/paste"
      scpaste-scp-pubkey "~/.ssh/uberspace_rsa.pub"
      scpaste-user-name "Profpatsch"
      scpaste-user-address "http://profpatsch.de")


;;;; Yasnippet
(ensure-package-installed 'yasnippet)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/snippets/yasnippet-snippets"))
;(add-hooks-to-mode 'prog-mode-hook'(yas-minor-mode
                                    ;yas-reload-all))
;; (defun ac-common-setup ()
;;   (setq ac-sources (add-to-list 'ac-sources 'ac-source-yasnippet)))
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)


;;;; Autopair
(ensure-package-installed 'autopair)
(require 'autopair)

;;;; fill-column-indicator
(ensure-package-installed 'fill-column-indicator)
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode)
(setq fci-rule-width 4)

;;;; FUEL (Factor)
;; from the official distribution, since the MELPA-version isn’t up-to-date
;(add-to-list 'load-path "/usr/lib/factor/misc/fuel")
;(ensure-package-installed 'fuel)
;(require 'factor-mode)


;;;; Flycheck

(ensure-package-installed 'flycheck 'flycheck-haskell)
(require 'flycheck)
(require 'flycheck-haskell)
(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-idle-check-delay 2)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import t)
 '(haskell-process-type (quote ghci)))


;;;; Emmet (fast XML)

(ensure-package-installed 'emmet-mode)
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;;; La Carte

;; Lets you see the existing menu items without using the GUI menu.
;; Run lacarte-execute-command and hit TAB
(ensure-package-installed 'lacarte)
(require 'lacarte)

(global-set-key (kbd "C-c m") 'lacarte-execute-command)

;;;; Ack
(ensure-package-installed 'ack)

;;;; Magit
(evil-leader/set-key
  "m" 'magit-status)


;;;; LANGUAGES



;;;; elisp
(add-hooks-to-mode 'emacs-lisp-mode-hook
		   '(enable-paredit-mode
		     turn-on-eldoc-mode))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;;;; Clojure
(ensure-package-installed 'cider)

(add-hook 'clojure-mode-hook 'paredit-mode)
(require 'cider)
(add-hooks-to-mode 'cider-mode-hook
		   '(cider-turn-on-eldoc-mode
		     subword-mode
		     paredit-mode))
(setq cider-popup-stacktraces nil) ;; Disable error popup when not in repl
(setq cider-repl-popup-stacktraces nil) ;; Disable error buffer in repl

;;; evaluate the ns form if cider is running (enabling autocompletion from the get-go)
(add-hook 'clojure-mode-hook (lambda ()
                               (when (member 'cider-mode minor-mode-list)
                                 (cider-eval-ns-form))))

(evil-define-key 'normal clojure-mode-map
  ;; "K" 'ac-nrepl-popup-doc ;; this is sweet, the docs pop up in the auto-complete box
  "]d" 'cider-src ;; I don’t know why I still use these awkward vim bindings …
  "]j" 'cider-javadoc
  )
(evil-define-key 'insert clojure-mode-map
  (kbd "C-j") 'cider-switch-to-relevant-repl-buffer)
(evil-leader/set-key-for-mode 'clojure-mode
  "d" 'clojure-fill-docstring
  "r" 'lisp-eval-region
  "t" 'clojure-jump-between-tests-and-code
  "j" 'cider-switch-to-relevant-repl-buffer
  "cj" 'cider-jack-in
  "cs" 'cider
  "pp" 'cider-pprint-eval-last-sexp
  )

(evil-define-key 'normal cider-mode-map
  ;; "K" 'ac-nrepl-popup-doc
  "]d" 'cider-src
  "]j" 'cider-javadoc
  (kbd "C-c C-e") (lambda () (interactive)
                    (evil-append 1)
                    (cider-eval-last-sexp)
                    (evil-normal-state)))
(evil-define-key 'insert cider-mode-map
  (kbd "C-j") 'cider-switch-to-last-clojure-buffer)
(evil-leader/set-key-for-mode 'cider-repl-mode
  "j" 'cider-switch-to-last-clojure-buffer
  "m" 'cider-macroexpand-1
  )

;;; completion
;; (ensure-package-installed 'ac-nrepl)
;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))

;; trigger in nrepl buffers
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; use ac-nrepl’s popup instead of nrepl-doc
;; (eval-after-load "cider"
;;   '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))


;;;; Python
(ensure-package-installed 'virtualenv )

(add-hooks-to-mode 'python-mode-hook
                   '(yas-minor-mode-on
                     (lambda ()
                       ;; (autopair-mode 't)
                       (setq fill-column 80))))

;;; python shell options
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;; folder with the virtualenvs
(setq virtualenv-root "~/.virtualenvs")

;;; jedi.el
;;; To make this work, go into the jedi dir and run `make requirements`.
;;; To make it work for py3k, change the virtualenv and python variables
;;; in the Makefile first. The other way around for systems with py3k
;;; default (Archlinux).
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; ;;; change jedi to the virtualenv with virtualenv.el
;; (add-hook 'virtualenv-minor-mode-hook
;;           ;; restart the jedi server in the venv
;;           (lambda ()
;;             (jedi:stop-server)
;;             (let ((args (append'("--virtual-env")
;;                                (list (concat (expand-file-name virtualenv-root) "/" virtualenv-workon-session "/")))))
;;               (setq jedi:server-args args))))

(evil-define-key 'normal python-mode-map
  "K" 'python-eldoc-function ;; this is sweet, the docs pop up in the auto-complete box
  )
(evil-leader/set-key-for-mode 'python-mode
  "r" 'python-shell-send-buffer
  )

;(ensure-package-installed 'flycheck-pyflakes)
;(add-hook 'python-mode-hook 'flycheck-mode)


;;;; Haskell

(ensure-package-installed 'haskell-mode)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(add-hooks-to-mode 'haskell-mode-hook
                   '(turn-on-haskell-indentation
                     turn-on-haskell-doc-mode))

(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-interactive-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(evil-define-key 'normal haskell-mode-map
  "K" 'haskell-process-do-info
  "]d" 'haskell-mode-jump-to-def)
(evil-leader/set-key-for-mode 'haskell-mode
  "r" 'haskell-process-load-or-reload
  "t" 'haskell-process-do-type
  "cb" 'haskell-process-cabal-build
  "n" 'next-error
  "p" 'previous-error)



;;;; Asciidoc
;; use Vim, stupid
(mapc (lambda (x) (add-to-list 'auto-mode-alist `(,x . visual-line-mode)))
      '("\\.asciidoc\\'" "\\.adoc\\'"))

;;;; Markdown
(ensure-package-installed 'markdown-mode)
(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(mapc (lambda (x) (add-to-list 'auto-mode-alist `(,x . markdown-mode)))
      '("\\.markdown\\'" "\\.md\\'" "\\.mdown\\'"))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-lines-mode)


;;;; Javascript

(ensure-package-installed 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;;; Coffeescript & Emberscript

(ensure-package-installed 'coffee-mode)
(require 'coffee-mode)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)
(add-to-list 'auto-mode-alist '("\\.em\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . html-mode))
(setq coffee-tab-width 4)

;;;; Go

(ensure-package-installed 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

(evil-define-key 'normal go-mode-map
  "]d" 'godef-jump
  "K" 'godoc 'word-at-point)

;;;; C

(evil-define-key 'normal c-mode-map
  "]d" (lambda () (interactive)
         (find-tag-other-window (symbol-name (symbol-at-point)))))

;;;; Scheme

(ensure-package-installed 'geiser)
(add-hook 'scheme-mode-hook 'enabe-paredit-mode)
(setq geiser-guile-load-init-file-p t)

;;;; Nix-Expressions
(eval-after-load 'tramp '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
(load "~/.emacs.d/scripts/nix-mode-1.0/nix-mode")
(require 'nix-mode)



;;;; Extempore

(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))


;;;; Vala
(add-hook 'vala-mode-hook (lambda ()
                           (setq indent-tabs-mode nil)
                           (setq c-basic-offset 4)
                           (setq tab-width 4)))


(provide '.emacs)
;;; .emacs ends here

;;;; GENERAL

;;; MELPA package repository
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)



;;;; LOOK
(set-face-attribute 'default nil :font "Inconsolata" :height 130)
(blink-cursor-mode 0)
(load-theme 'solarized-dark t)
(require 'powerline)
(powerline-center-evil-theme)

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
(global-set-key (kbd "RET") 'newline-and-indent) ;; Return should indent
(setq undo-limit 3600) ;; Some more undo steps
(defalias 'yes-or-no-p 'y-or-n-p) ;; No typing yes please
(setq-default indent-tabs-mode nil) ;; Death to non-smart tabs

;;;; Save on focus lost in X11
;;; depreciates in Emacs 24.4
(when
   (and (featurep 'x) window-system)
 (defvar on-blur--saved-window-id 0 "Last known focused window.")
 (defvar on-blur--timer nil "Timer refreshing known focused window.")
 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (let* ((active-window (x-window-property
                          "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
          (active-window-id (if (numberp active-window)
                                active-window
                              (string-to-number
                               (format "%x00%x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
          (emacs-window-id (string-to-number
                            (frame-parameter nil 'outer-window-id))))
     (when (and
            (= emacs-window-id on-blur--saved-window-id)
            (not (= active-window-id on-blur--saved-window-id)))
       (run-hooks 'on-blur-hook))
     (setq on-blur--saved-window-id active-window-id)
     (run-with-timer 1 nil 'on-blur--refresh)))
 (add-hook 'on-blur-hook #'(lambda () (save-some-buffers t)))
 (on-blur--refresh))

;;;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

;;; shadows find-file with this dispatch-lambda
(setq native-find-file-func (symbol-function 'find-file)) ;; original find-file code
(fset 'find-file (lambda (filename &optional wildcards)
                   (if filename
                       (native-find-file-func filename wildcards)
                     (ido-find-file))))
(defalias 'switch-buffer 'ido-switch-buffer) ;; use better buffer switcher

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
;(deffacedd paren-match) TODO
;(set-face-background 'paren-match "#000000")
;(set-face-background 'show-paren-match "#073642")
;(set-face-foreground 'show-paren-match (face-foreground 'default))

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
;;;; auto-complete

(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu t) ;; Don’t show the menu automatically
(setq ac-auto-start 4)       ;; Autocomplete after 4 letters
(define-key ac-mode-map (kbd "TAB") 'auto-complete)


;;;; LANGUAGES

(defun add-hooks-to-mode (mode-hook functions)
  (dolist (hook functions)
    (add-hook mode-hook hook)))

;;;; elisp
(add-hooks-to-mode 'emacs-lisp-mode-hook
		   '(enable-paredit-mode
		     turn-on-eldoc-mode))
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;; Clojure
(require 'cider)
(add-hooks-to-mode 'cider-mode-hook
		   '(cider-turn-on-eldoc-mode
		     subword-mode
		     paredit-mode))
(setq cider-popup-stacktraces nil) ;; Disable error popup when not in repl
(setq cider-repl-popup-stacktraces nil) ;; Disable error buffer in repl

;;;; Asciidoc
;; use Vim, stupid

;;;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))


;;; completion
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; trigger in nrepl buffers
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; use ac-nrepl’s popup instead of nrepl-doc
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;;;; PLUGINS

;;;; Evil (Vim)
(require 'evil-leader) ;; Needs execution order to work in every buffer …
(require 'evil)
; Normal State
(defun evil-empty-line-below ()
  (interactive)
  (evil-open-below 0)
  (evil-normal-state))

(defun describe-function-at-point-active-window ()
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (describe-function symb))
    (other-window 1)))

(setq normal-state-mappings '(
    ("RET" . evil-empty-line-below)
    ("K"   . describe-function-at-point-active-window)
    ("]d"  . find-function-at-point)))

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
(global-evil-leader-mode)
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



;;;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(smex-auto-update 60)
(setq smex-save-file "~/.emacs.d/smex-items")


;;;; scpaste
(require 'scpaste)
(setq scpaste-http-destination "https://bigmac.caelum.uberspace.de/paste"
      scpaste-scp-destination "bigmac@uberspace:html/paste"
      scpaste-user-name "Profpatsch"
      scpaste-user-address "http://profpatsch.de")

;;;; eldoc
(eldoc-mode 1) ; enable globally

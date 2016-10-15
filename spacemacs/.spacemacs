;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; emacs
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.config/spacemacs/snippets")
     ; markup
     html
     org
     markdown
     ; languages
     emacs-lisp
     haskell
     ;; go
     javascript
     ;purescript
     shell-scripts
     nixos
     ;scala
     ; emacs
     syntax-checking
     themes-megapack
     ; misc
     git
     ;rust
     
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
     '(graphviz-dot-mode paredit nix-sandbox magit-annex magit-gh-pulls)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(smartparens)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         graham
                         birds-of-paradise-plus
                         junio
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "C-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-,"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; first things first
  (setq lexical-binding t)

  ;;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  ;;; set meta to alt key
  (setq x-super-keysym 'meta)
  ;;;; Save on focus lost in X11
  (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

  ;; org-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (with-eval-after-load 'org
    (setq org-startup-indented nil))

  ;; comany-mode
  ;;;; complete with right arrow
  ;; (define-key company-active-map (kbd "right") 'company-complete-selection)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)))
  ;; don’t ask for dot
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; maven
  (define-minor-mode maven-mode "")
  (add-hook 'nxml-mode-hook 'maven-mode)
  (add-hook 'maven-mode-hook (lambda () (yas-activate-extra-mode 'maven-mode)))

  ;; haskell with nix
  (setq haskell-process-wrapper-function
        (lambda (args) apply 'nix-shell-command (nix-current-sandbox) args))

  (setq flycheck-command-wrapper-function
        (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
        flycheck-executable-find
        (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

  (defun my-ghc-mod-setup ()
    (set (make-local-variable 'my-ghc-mod) (nix-executable-find (nix-current-sandbox) "ghc-mod"))
    (message "local ghc-mod is: %S" my-ghc-mod)
    (set (make-local-variable 'ghc-module-command) my-ghc-mod)
    (set (make-local-variable 'ghc-command) my-ghc-mod))
  (add-hook 'haskell-mode-hook 'my-ghc-mod-setup)


  ;; (setq ghc-commmand (concat "nix-shell " (projectile-project-root) " --run ghc"))
  ;; (defun nix-shell-cmd (cmd)
  ;;   (let ((additional '(ghc-mod stack)))
  ;;     `("nix-shell" "-E 'with (import <nixpkgs> {}); with haskellPackages; haskell.lib.addBuildTools (callPackage" ,(projectile-project-root) "{}) [" ,@additional ,(car cmd) "] --run" ,@cmd "'")))
  ;; (defun my-nix-shell-flycheck-command ()
  ;;   (set (make-local-variable 'flycheck-command-wrapper-function)
  ;;        'nix-shell-cmd))
  ;; (defun my-nix-shell-ghc-command ()
  ;;   (set (make-local-variable 'ghc-command)
  ;;        (nix-shell-cmd '("ghc-mod"))))
  ;; (add-hook 'haskell-mode-hook 'my-nix-shell-flycheck-command)
  ;; (add-hook 'haskell-mode-hook 'my-nix-shell-ghc-command)

;;   (defun my-cons-to-local-exec-path (path)
;;     "Add a path to the buffer-local exec-path."
;;     (set (make-local-variable 'exec-path) (cons path exec-path)))

;;   (defun my-nix-build-attribute (packageset attribute action)
;;     "Build the given ATTRIBUTE in the nix PACKAGESET with nix-build.

;; ACTION is a function that takes a PATH parameter and is called when
;; nix-build finishes."
;;     (let ((process-connection-type nil) ; use a pipe
;;           (process (start-process "nix-build" "*nix-build*" "nix-build" "-A" attribute packageset)))
;;       (set-process-sentinel
;;        process
;;        (lambda (process event)
;;          (cond ((string-equal event "finished\n")
;;                 ;; call the action with the store path
;;                 (funcall action
;;                          (my-buffer-last-line (process-buffer process)))))))))

;;   (defun my-nix-build-attribute-sync (packageset attribute)
;;     "Build the given ATTRIBUTE in the nix PACKAGESET with nix-build.
;; Return the resulting store path."
;;     (let ((buffer (get-buffer-create "*nix-build*")))
;;       (with-current-buffer buffer
;;         (progn
;;           (call-process "nix-build" nil t t "-A" attribute packageset)
;;           (my-buffer-last-line buffer)))))

;;   (defun my-add-nix-build-to-environment (packageset attribute)
;;     "Helper, add the bin directory of the derivation’s store path to
;; the local exec-path."
;;     (my-cons-to-local-exec-path
;;      (concat (my-nix-build-attribute-sync packageset attribute) "/bin")))

;;   (defun my-buffer-last-line (buffer)
;;     "Returns the last line of BUFFER, ignores a final linebreak."
;;     (with-current-buffer buffer
;;       (save-excursion
;;         (let ((eof (point-max)))
;;           (progn
;;             (goto-char eof)
;;             ;; We want to ignore a trailing newline
;;             (if (= (line-beginning-position) eof)
;;                 (goto-char (- eof 1)))
;;             (buffer-substring-no-properties
;;              (line-beginning-position) (line-end-position)))))))

;;   (defun my-ghc-mod-env ()
;;     (my-add-nix-build-to-environment "<nixpkgs>" "haskellPackages.ghc-mod"))
;;   (defun my-stack-env ()
;;     (my-add-nix-build-to-environment "<nixpkgs>" "haskellPackages.stack"))
  ;; (add-hook 'haskell-mode-hook 'my-ghc-mod-env)
  ;; (add-hook 'haskell-mode-hook 'my-stack-env)


  ;; nice glyphs for haskell with hasklig
  (custom-set-variables '(haskell-font-lock-symbols t)
                        '(haskell-font-lock-symbols-alist
                          (and (fboundp 'decode-char)
                              (list (cons "&&" (decode-char 'ucs #XE100))
                                    (cons "***" (decode-char 'ucs #XE101))
                                    (cons "*>" (decode-char 'ucs #XE102))
                                    (cons "\\\\" (decode-char 'ucs #XE103))
                                    (cons "||" (decode-char 'ucs #XE104))
                                    (cons "|>" (decode-char 'ucs #XE105))
                                    (cons "::" (decode-char 'ucs #XE106))
                                    (cons "==" (decode-char 'ucs #XE107))
                                    (cons "===" (decode-char 'ucs #XE108))
                                    (cons "==>" (decode-char 'ucs #XE109))
                                    (cons "=>" (decode-char 'ucs #XE10A))
                                    (cons "=<<" (decode-char 'ucs #XE10B))
                                    (cons "!!" (decode-char 'ucs #XE10C))
                                    (cons ">>" (decode-char 'ucs #XE10D))
                                    (cons ">>=" (decode-char 'ucs #XE10E))
                                    (cons ">>>" (decode-char 'ucs #XE10F))
                                    (cons ">>-" (decode-char 'ucs #XE110))
                                    (cons ">-" (decode-char 'ucs #XE111))
                                    (cons "->" (decode-char 'ucs #XE112))
                                    (cons "-<" (decode-char 'ucs #XE113))
                                    (cons "-<<" (decode-char 'ucs #XE114))
                                    (cons "<*" (decode-char 'ucs #XE115))
                                    (cons "<*>" (decode-char 'ucs #XE116))
                                    (cons "<|" (decode-char 'ucs #XE117))
                                    (cons "<|>" (decode-char 'ucs #XE118))
                                    (cons "<$>" (decode-char 'ucs #XE119))
                                    (cons "<>" (decode-char 'ucs #XE11A))
                                    (cons "<-" (decode-char 'ucs #XE11B))
                                    (cons "<<" (decode-char 'ucs #XE11C))
                                    (cons "<<<" (decode-char 'ucs #XE11D))
                                    (cons "<+>" (decode-char 'ucs #XE11E))
                                    (cons ".." (decode-char 'ucs #XE11F))
                                    (cons "..." (decode-char 'ucs #XE120))
                                    (cons "++" (decode-char 'ucs #XE121))
                                    (cons "+++" (decode-char 'ucs #XE122))
                                    (cons "/=" (decode-char 'ucs #XE123))))))

  ;; (defconst fira-code-font-lock-keywords-alist
  ;;   (mapcar (lambda (regex-char-pair)
  ;;             `(,(car regex-char-pair)
  ;;               (0 (prog1 ()
  ;;                   (compose-region (match-beginning 1)
  ;;                                   (match-end 1)
  ;;                                   ;; The first argument to concat is a string containing a literal tab
  ;;                                   ,(concat "   " (list (decode-char 'ucs (cadr regex-char-pair)))))))))
  ;;           '(("\\(www\\)"                   #Xe100)
  ;;             ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
  ;;             ("\\(\\*\\*\\*\\)"             #Xe102)
  ;;             ("\\(\\*\\*/\\)"               #Xe103)
  ;;             ("\\(\\*>\\)"                  #Xe104)
  ;;             ("[^*]\\(\\*/\\)"              #Xe105)
  ;;             ("\\(\\\\\\\\\\)"              #Xe106)
  ;;             ("\\(\\\\\\\\\\\\\\)"          #Xe107)
  ;;             ("\\({-\\)"                    #Xe108)
  ;;             ("\\(\\[\\]\\)"                #Xe109)
  ;;             ("\\(::\\)"                    #Xe10a)
  ;;             ("\\(:::\\)"                   #Xe10b)
  ;;             ("[^=]\\(:=\\)"                #Xe10c)
  ;;             ("\\(!!\\)"                    #Xe10d)
  ;;             ("\\(!=\\)"                    #Xe10e)
  ;;             ("\\(!==\\)"                   #Xe10f)
  ;;             ("\\(-}\\)"                    #Xe110)
  ;;             ("\\(--\\)"                    #Xe111)
  ;;             ("\\(---\\)"                   #Xe112)
  ;;             ("\\(-->\\)"                   #Xe113)
  ;;             ("[^-]\\(->\\)"                #Xe114)
  ;;             ("\\(->>\\)"                   #Xe115)
  ;;             ("\\(-<\\)"                    #Xe116)
  ;;             ("\\(-<<\\)"                   #Xe117)
  ;;             ("\\(-~\\)"                    #Xe118)
  ;;             ("\\(#{\\)"                    #Xe119)
  ;;             ("\\(#\\[\\)"                  #Xe11a)
  ;;             ("\\(##\\)"                    #Xe11b)
  ;;             ("\\(###\\)"                   #Xe11c)
  ;;             ("\\(####\\)"                  #Xe11d)
  ;;             ("\\(#(\\)"                    #Xe11e)
  ;;             ("\\(#\\?\\)"                  #Xe11f)
  ;;             ("\\(#_\\)"                    #Xe120)
  ;;             ("\\(#_(\\)"                   #Xe121)
  ;;             ("\\(\\.-\\)"                  #Xe122)
  ;;             ("\\(\\.=\\)"                  #Xe123)
  ;;             ("\\(\\.\\.\\)"                #Xe124)
  ;;             ("\\(\\.\\.<\\)"               #Xe125)
  ;;             ("\\(\\.\\.\\.\\)"             #Xe126)
  ;;             ("\\(\\?=\\)"                  #Xe127)
  ;;             ("\\(\\?\\?\\)"                #Xe128)
  ;;             ("\\(;;\\)"                    #Xe129)
  ;;             ("\\(/\\*\\)"                  #Xe12a)
  ;;             ("\\(/\\*\\*\\)"               #Xe12b)
  ;;             ("\\(/=\\)"                    #Xe12c)
  ;;             ("\\(/==\\)"                   #Xe12d)
  ;;             ("\\(/>\\)"                    #Xe12e)
  ;;             ("\\(//\\)"                    #Xe12f)
  ;;             ("\\(///\\)"                   #Xe130)
  ;;             ("\\(&&\\)"                    #Xe131)
  ;;             ("\\(||\\)"                    #Xe132)
  ;;             ("\\(||=\\)"                   #Xe133)
  ;;             ("[^|]\\(|=\\)"                #Xe134)
  ;;             ("\\(|>\\)"                    #Xe135)
  ;;             ("\\(\\^=\\)"                  #Xe136)
  ;;             ("\\(\\$>\\)"                  #Xe137)
  ;;             ("\\(\\+\\+\\)"                #Xe138)
  ;;             ("\\(\\+\\+\\+\\)"             #Xe139)
  ;;             ("\\(\\+>\\)"                  #Xe13a)
  ;;             ("\\(=:=\\)"                   #Xe13b)
  ;;             ("[^!/]\\(==\\)[^>]"           #Xe13c)
  ;;             ("\\(===\\)"                   #Xe13d)
  ;;             ("\\(==>\\)"                   #Xe13e)
  ;;             ("[^=]\\(=>\\)"                #Xe13f)
  ;;             ("\\(=>>\\)"                   #Xe140)
  ;;             ("\\(<=\\)"                    #Xe141)
  ;;             ("\\(=<<\\)"                   #Xe142)
  ;;             ("\\(=/=\\)"                   #Xe143)
  ;;             ("\\(>-\\)"                    #Xe144)
  ;;             ("\\(>=\\)"                    #Xe145)
  ;;             ("\\(>=>\\)"                   #Xe146)
  ;;             ("[^-=]\\(>>\\)"               #Xe147)
  ;;             ("\\(>>-\\)"                   #Xe148)
  ;;             ("\\(>>=\\)"                   #Xe149)
  ;;             ("\\(>>>\\)"                   #Xe14a)
  ;;             ("\\(<\\*\\)"                  #Xe14b)
  ;;             ("\\(<\\*>\\)"                 #Xe14c)
  ;;             ("\\(<|\\)"                    #Xe14d)
  ;;             ("\\(<|>\\)"                   #Xe14e)
  ;;             ("\\(<\\$\\)"                  #Xe14f)
  ;;             ("\\(<\\$>\\)"                 #Xe150)
  ;;             ("\\(<!--\\)"                  #Xe151)
  ;;             ("\\(<-\\)"                    #Xe152)
  ;;             ("\\(<--\\)"                   #Xe153)
  ;;             ("\\(<->\\)"                   #Xe154)
  ;;             ("\\(<\\+\\)"                  #Xe155)
  ;;             ("\\(<\\+>\\)"                 #Xe156)
  ;;             ("\\(<=\\)"                    #Xe157)
  ;;             ("\\(<==\\)"                   #Xe158)
  ;;             ("\\(<=>\\)"                   #Xe159)
  ;;             ("\\(<=<\\)"                   #Xe15a)
  ;;             ("\\(<>\\)"                    #Xe15b)
  ;;             ("[^-=]\\(<<\\)"               #Xe15c)
  ;;             ("\\(<<-\\)"                   #Xe15d)
  ;;             ("\\(<<=\\)"                   #Xe15e)
  ;;             ("\\(<<<\\)"                   #Xe15f)
  ;;             ("\\(<~\\)"                    #Xe160)
  ;;             ("\\(<~~\\)"                   #Xe161)
  ;;             ("\\(</\\)"                    #Xe162)
  ;;             ("\\(</>\\)"                   #Xe163)
  ;;             ("\\(~@\\)"                    #Xe164)
  ;;             ("\\(~-\\)"                    #Xe165)
  ;;             ("\\(~=\\)"                    #Xe166)
  ;;             ("\\(~>\\)"                    #Xe167)
  ;;             ("[^<]\\(~~\\)"                #Xe168)
  ;;             ("\\(~~>\\)"                   #Xe169)
  ;;             ("\\(%%\\)"                    #Xe16a)
  ;;           ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
  ;;             ("[^:=]\\(:\\)[^:=]"           #Xe16c)
  ;;             ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
  ;;             ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

  ;; (defun add-fira-code-symbol-keywords ()
  ;;   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

  ;; (add-hook 'prog-mode-hook
  ;;           #'add-fira-code-symbol-keywords)


  ;; nix-mode
  (defun my-insert-random-sha256 ()
    (interactive)
    (-> (concat "echo '" (int-to-string (random)) "' | sha256sum | cut -d' ' -f1")
        (shell-command-to-string)
        (s-trim-right)
        (insert)))
  (spacemacs/set-leader-keys-for-major-mode 'nix-mode
    "s" 'my-insert-random-sha256)

)


;; do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color nil)
 '(cua-normal-cursor-color nil)
 '(cua-overwrite-cursor-color "")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#eee8d5" t)
 '(flycheck-pos-tip-mode t)
 '(flycheck-pos-tip-timeout 0)
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(graphviz-dot-view-command "dot -T png %s | feh -")
 '(haskell-font-lock-symbols t)
 '(haskell-font-lock-symbols-alist
   (and
    (fboundp
     (quote decode-char))
    (list
     (cons "&&"
           (decode-char
            (quote ucs)
            57600))
     (cons "***"
           (decode-char
            (quote ucs)
            57601))
     (cons "*>"
           (decode-char
            (quote ucs)
            57602))
     (cons "\\\\"
           (decode-char
            (quote ucs)
            57603))
     (cons "||"
           (decode-char
            (quote ucs)
            57604))
     (cons "|>"
           (decode-char
            (quote ucs)
            57605))
     (cons "::"
           (decode-char
            (quote ucs)
            57606))
     (cons "=="
           (decode-char
            (quote ucs)
            57607))
     (cons "==="
           (decode-char
            (quote ucs)
            57608))
     (cons "==>"
           (decode-char
            (quote ucs)
            57609))
     (cons "=>"
           (decode-char
            (quote ucs)
            57610))
     (cons "=<<"
           (decode-char
            (quote ucs)
            57611))
     (cons "!!"
           (decode-char
            (quote ucs)
            57612))
     (cons ">>"
           (decode-char
            (quote ucs)
            57613))
     (cons ">>="
           (decode-char
            (quote ucs)
            57614))
     (cons ">>>"
           (decode-char
            (quote ucs)
            57615))
     (cons ">>-"
           (decode-char
            (quote ucs)
            57616))
     (cons ">-"
           (decode-char
            (quote ucs)
            57617))
     (cons "->"
           (decode-char
            (quote ucs)
            57618))
     (cons "-<"
           (decode-char
            (quote ucs)
            57619))
     (cons "-<<"
           (decode-char
            (quote ucs)
            57620))
     (cons "<*"
           (decode-char
            (quote ucs)
            57621))
     (cons "<*>"
           (decode-char
            (quote ucs)
            57622))
     (cons "<|"
           (decode-char
            (quote ucs)
            57623))
     (cons "<|>"
           (decode-char
            (quote ucs)
            57624))
     (cons "<$>"
           (decode-char
            (quote ucs)
            57625))
     (cons "<>"
           (decode-char
            (quote ucs)
            57626))
     (cons "<-"
           (decode-char
            (quote ucs)
            57627))
     (cons "<<"
           (decode-char
            (quote ucs)
            57628))
     (cons "<<<"
           (decode-char
            (quote ucs)
            57629))
     (cons "<+>"
           (decode-char
            (quote ucs)
            57630))
     (cons ".."
           (decode-char
            (quote ucs)
            57631))
     (cons "..."
           (decode-char
            (quote ucs)
            57632))
     (cons "++"
           (decode-char
            (quote ucs)
            57633))
     (cons "+++"
           (decode-char
            (quote ucs)
            57634))
     (cons "/="
           (decode-char
            (quote ucs)
            57635)))))
 '(haskell-indent-spaces 4)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-babel-load-languages (quote ((shell . t) (dot . t))))
 '(org-clock-clocktable-default-properties (quote (:maxlevel 4 :scope file)))
 '(org-confirm-babel-evaluate nil)
 '(org-html-html5-fancy t)
 '(org-html-indent t)
 '(org-structure-template-alist
   (quote
    (("n" "#+NAME: ")
     ("s" "#+BEGIN_SRC ?

#+END_SRC")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX")
     ("L" "#+LaTeX: ")
     ("h" "#+BEGIN_HTML
?
#+END_HTML")
     ("H" "#+HTML: ")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII")
     ("A" "#+ASCII: ")
     ("i" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?"))))
 '(package-selected-packages
   (quote
    (pug-mode f org-projectile org-download intero helm-hoogle git-link evil-ediff dumb-jump request simple-httpd package-build zenburn-theme ws-butler window-numbering web-mode ujelly-theme tao-theme spacemacs-theme spaceline smooth-scrolling shm planet-theme persp-mode page-break-lines orgit organic-green-theme org-repo-todo org-pomodoro alert org-plus-contrib open-junk-file omtose-phellack-theme nix-sandbox neotree naquadah-theme monokai-theme moe-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow leuven-theme less-css-mode js2-refactor js2-mode indent-guide hl-todo hindent help-fns+ helm-themes helm-projectile helm-make projectile helm-descbinds helm-c-yasnippet helm-ag haskell-snippets gruvbox-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate git-messenger expand-region exec-path-from-shell evil-surround evil-search-highlight-persist evil-mc evil-matchit evil-magit evil-iedit-state iedit evil-exchange emmet-mode dracula-theme darktooth-theme company-quickhelp color-theme-sanityinc-tomorrow buffer-move bracketed-paste badwolf-theme auto-yasnippet yasnippet auto-compile apropospriate-theme anti-zenburn-theme ample-theme ace-link ace-jump-helm-line auto-complete avy ghc anzu smartparens haskell-mode flycheck company helm helm-core magit magit-popup git-commit with-editor gh marshal pcache ht hydra s use-package which-key evil dash zonokai-theme zen-and-art-theme web-beautify volatile-highlights vi-tilde-fringe uuidgen underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powerline popwin popup pkg-info phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paredit paradox packed org-present org-bullets oldlace-theme occidental-theme obsidian-theme noctilux-theme nix-mode niflheim-theme mustang-theme multiple-cursors move-text monochrome-theme molokai-theme mmm-mode minimal-theme magit-gh-pulls magit-annex macrostep lush-theme lorem-ipsum logito log4e livid-mode linum-relative link-hint light-soap-theme json-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ ido-vertical-mode hungry-delete htmlize hlint-refactor highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-swoop helm-nixos-options helm-mode-manager helm-gitignore helm-flx helm-css-scss helm-company hc-zenburn-theme gruber-darker-theme goto-chg golden-ratio gnuplot gntp gitconfig-mode gitattributes-mode git-timemachine gh-md gandalf-theme flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-numbers evil-nerd-commenter evil-lisp-state evil-indent-plus evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme elisp-slime-nav django-theme diminish define-word darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-shell company-nixos-options company-ghci company-ghc company-cabal column-enforce-mode colorsarenice-theme color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key auto-highlight-symbol async ample-zen-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(pos-tip-use-relative-coordinates t)
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 25)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval company-mode -42)
     (eval company-mode nil)
     (hindent-style . "chris-done"))))
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy scala-indent:operator-strategy)
 '(select-enable-clipboard nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tooltip-hide-delay 0)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

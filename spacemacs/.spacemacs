;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; emacs

     ; spacemacs core
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.config/spacemacs/scompany-backends-LaTeX-modenippets")
     ; markup
     ;; html
     ;; (org :variables
     ;;      org-enable-reveal-js-support t)
     markdown
     ; languages
     (rust :variables
           rust-backend "lsp")
     lsp
     ;; emacs-lisp
     ; haskell-mode is kinda broken with dante and lsp, so I just want syntax highlighting
     ;; (haskell :variables haskell-completion-backend 'dante)
     ;; ocaml
     ;; elm
     go
     forth
     ;; javascript
     purescript
     nixos
     ;; graphviz
     ;scala
     ; emacs
     syntax-checking
     multiple-cursors
     ;; latex
     themes-megapack
     ; misc
     git
     pdf
     ; email
     (mu4e :variables
           mu4e-use-maildirs-extension t)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     graphviz-dot-mode
     dhall-mode
     direnv
     haskell-mode
     tree-sitter
     diff-hl
     counsel-jq
     frames-only-mode
     ;; explain-pause-mode
     )
   ;; A list of packhttps://shop.spreadshirt.com/spacemacs-shop/ages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
     smartparens
      ; sucks, can be disabled
     company-auctex
     ; this acts like ESC in insert mode when you hit fd in .1 sec
     ; kind of distracting for my typing speed & working with file handles. :P
     evil-escape
     ;; i hate it
     yasnippet yasnippet-auto
   )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         birds-of-paradise-plus
                         ;; solarized-light
                         ;; junio
                         ;; molokai
                         ;; sanityinc-tomorrow-blue
                         ;; graham
                         ;; railscasts
                         ;; stopped at molokai
                         )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.

   dotspacemacs-default-font '("Hasklig" :size 16.0 :weight normal :width normal :powerline-scale 1.2)
   ;; dotspacemacs-default-font '(("Fira Code" :size 15 :weight normal :width normal :powerline-scale 1.1)
   ;;                             ("Fira Code Symbol" :size 15))

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "C-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server 't

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."

  ;; I don’t need this feature, because I use lorri/direnv
  ;(spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-to-list 'load-path "/home/philip/kot/emacs/include")
  (add-to-list 'load-path "/home/philip/depot/tools/emacs-pkgs/tvl")
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)


(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
Layers configuration."

  ;; first things first
  (setq lexical-binding t)

  ;; replace most emacs windows by frames, so they can be managed by the window manager
  (frames-only-mode)

  ;;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  ;;; set meta to alt key
  (setq x-super-keysym 'meta)
  ;;;; Save on focus lost in X11
  (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

  ;;; fixes

  ; magit rebase mode
  ; https://github.com/syl20bnr/spacemacs/issues/15089#issuecomment-984207351
  (setq auto-mode-alist (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode) auto-mode-alist))

  ;;; tmp

  (require 'git-link)
  (defun git-link-commit-cgit (hostname dirname commit)
    (format "https://%s/%s/commit/?id=%s"
	          hostname
	          dirname
	          commit))

  (defun git-link-commit-cgit-tvl (hostname dirname commit)
    ;; tvl depot does not have a dirname in its url
    (format "https://%s/commit/?id=%s"
	          hostname
	          commit))

  (add-to-list 'git-link-commit-remote-alist '("code.sterni.lv" git-link-commit-cgit))
  (add-to-list 'git-link-commit-remote-alist '("code.tvl.fyi" git-link-commit-cgit-tvl))

  ;; https://code.sterni.lv/sm/tree/inet/gopher/hierarchy.nix?id=f9b7232bd359b6d27303e44dc539259918612615#n12
  (defun git-link-cgit (hostname dirname filename branch commit start end)
    (format "https://%s/%s/tree/%s?id=%s#n%s"
            hostname
            dirname
            filename
            commit
            start
            ;; no end possible?
            ))

  (defun git-link-cgit-tvl (hostname dirname filename branch commit start end)
    ;; tvl depot does not have a dirname in its url
    (format "https://%s/tree/%s?id=%s#n%s"
            hostname
            filename
            commit
            start
            ;; no end possible?
            ))

  (add-to-list 'git-link-remote-alist '("code.sterni.lv" git-link-cgit))
  (add-to-list 'git-link-remote-alist '("code.tvl.fyi" git-link-cgit-tvl))

  ;;; copy (kill-ring-save) & paste (yank)
  (global-set-key (kbd "C-<f12>") 'clipboard-kill-ring-save)
  (global-set-key (kbd "C-<f11>") 'clipboard-yank)

  ;;; movement

  (defun my-slurp-and-follow ()
    (interactive)
    (sp-forward-slurp-sexp)
    (forward-sexp))
  (define-key evil-insert-state-map (kbd "C-s") 'my-slurp-and-follow)

  ;; (define-key evil-normal-state-map (kbd "C-<right>") 'sp-forward-parallel-sexp)
  ;; (define-key evil-normal-state-map (kbd "C-<left>") 'sp-backward-parallel-sexp)
  ;; (define-key evil-normal-state-map (kbd "C-<down>") 'sp-down-sexp)
  ;; (define-key evil-normal-state-map (kbd "C-<up>") 'sp-backward-up-sexp)

  ;;; tree sitter

  (require 'tree-sitter)

  ;;; unsets
  (defun spacemacs/browse-docs-online-at-point (&rest args)
    (interactive "P"))

  ;;; depot depot depot
  (require 'tvl)

  ;;; user functions
  (defun current-directory-xdg-open ()
    "Pass the current directory (pwd) to the \"xdg-open\" command."
    (interactive)
    (call-process "xdg-open" nil 0 nil default-directory))
  (spacemacs/set-leader-keys
    "oo" 'current-directory-xdg-open)

  ;;; direnv integration
  (add-hook 'prog-mode-hook
            (lambda () (direnv-mode)))

  ;; text-mode

  (add-hook 'text-mode-hook
            (lambda () (set-fill-column 70)))

  ;; org-mode
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook
            (lambda () (spacemacs/toggle-auto-completion-off)))
  (with-eval-after-load 'org
    (setq org-startup-indented nil))

  ;; markdown-mod  ;; nxml-mode
  (add-hook 'nxml-mode-hook 'auto-fill-mode)

  ;; yaml files
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))

  ;; terraform files
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . conf-mode))

  ;; ninja files
  (add-to-list 'auto-mode-alist '("\\.ninja\\'" . conf-mode))

  ;; Typescript/react files
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . javascript-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))

  ;; company-mode
  ; delete with C-w, even when completion window is open
  ;(define-key company-active-map (kbd "C-w") 'evil-delete-backward-word) TODO: company-active-map not defined?

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)))
  ;; don’t ask for dot
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; rust-mode
  ; Otherwise racer picks up the environment variable only one time,
  ; which doesn’t play nicely with direnv.
  (setq racer-rust-src-path nil)
  (setq racer-cmd nil)

  ;; bazel

  (define-derived-mode bazel-mode python-mode "Bazel"
    "Major mode for editing Bazel files."
    :group 'bazel

    (setq-local comment-use-syntax t)
    (setq-local comment-start "#")
    (setq-local comment-end "")
    (setq-local indent-tabs-mode nil))

  (add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))

  ;; maven
  (define-minor-mode maven-mode "")
  (add-hook 'nxml-mode-hook 'maven-mode)
  ;; (add-hook 'maven-mode-hook (lambda () (yas-activate-extra-mode 'maven-mode)))

  ;; Deprecated by lorri
  ;; (setq flycheck-command-wrapper-function
  ;;         (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;;       flycheck-executable-find
  ;;         (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

  ;; TODO: replace by lorri
  (defun my-ghc-mod-setup ()
    (set (make-local-variable 'my-ghc-mod) (nix-executable-find (nix-current-sandbox) "ghc-mod"))
    (message "local ghc-mod is: %S" my-ghc-mod)
    (set (make-local-variable 'ghc-module-command) my-ghc-mod)
    (set (make-local-variable 'ghc-command) my-ghc-mod))
  ;; TODO: this blooooocks
  ;; (add-hook 'haskell-mode-hook 'my-ghc-mod-setup)

  (defun my-correct-symbol-bounds (pretty-alist)
    "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
    (mapcar (lambda (el)
              (setcdr el (string ?\t (cdr el)))
              el)
            pretty-alist))

  (defun my-ligature-list (ligatures codepoint-start)
    "Create an alist of strings to replace with
codepoints starting from codepoint-start."
    (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
      (-zip-pair ligatures codepoints)))

  ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
  (setq my-hasklig-ligatures
    (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                   "==" "===" "==>" "=>" "=<<" "!!" ">>"
                   ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                   "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                   "<<" "<<<" "<+>" ".." "..." "++" "+++"
                   "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

  
  (setq my-fira-code-ligatures
    (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                  "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                  "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                  "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                  ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                  "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                  "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                  "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                  ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                  "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                  "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                  "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                  "x" ":" "+" "+" "*")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

  ;; nice glyphs for haskell with hasklig
  (defun my-set-hasklig-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-hasklig-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))
  (defun my-set-fira-code-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-fira-code-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))

  (add-hook 'haskell-mode-hook 'my-set-hasklig-ligatures)

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

  ; TODO
  ;; (defun mwheel-filter-click-events ()
  ;;   "Discard `mouse-wheel-click-event' while scrolling the mouse."
  ;;   (if (cl-some (lambda (ev) (event-basic-type last-input-event) ev)
  ;;                `(mouse-wheel-click-event
  ;;                  ,(intern "mouse-6")
  ;;                  ,(intern "mouse-7")))
  ;;       (setq this-command 'ignore)))


  ;; nix-mode
  (require 'nix-prefetch)
  (defun my-insert-random-sha256 ()
    (interactive)
    (-> (concat "echo '" (int-to-string (random)) "' | sha256sum | cut -d' ' -f1")
        (shell-command-to-string)
        (s-trim-right)
        (insert)))
  (defun my-nix-yank-prefetch-url-unpack ()
    (interactive)
    (nix-prefetch-yank-hash 'nix-prefetch-url-unpack))
  (spacemacs/set-leader-keys-for-major-mode 'nix-mode
    "s" 'my-insert-random-sha256
    "U" 'my-nix-yank-prefetch-url-unpack)

  ;; add numbers in selection
  (defun exchange-point-and-mark-nomark ()
    (exchange-point-and-mark nil))
  (defun calculator-sum-column (start end)
    "Adds numbers in rectangle."
    (interactive "r")
    (save-excursion
      (kill-rectangle start end)
      (exchange-point-and-mark)
      (yank-rectangle)
      (set-buffer (get-buffer-create "*calc-sum*"))
      (erase-buffer)
      (yank-rectangle)
      (exchange-point-and-mark-nomark)
      (let ((sum 0))
        (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
	        (setq sum (+ sum (string-to-number (match-string 0)))))
        (message "Sum: %f" sum))))

  ;; mu4e
  (require 'mu4e)
  ; by default it rebinds to beginning/end-of-buffer which is different from everywhere else
  (define-key mu4e-view-mode-map (kbd "<home>") 'move-beginning-of-line)
  (define-key mu4e-view-mode-map (kbd "<end>") 'move-end-of-line)
  (setq mail-user-agent 'mu4e-user-agent
        mu4e-confirm-quit nil

        ; fancy chars make the entries jump around
        mu4e-use-fancy-chars nil

        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil
        mu4e-compose-complete-only-after "2018-01-01"

        ; too much noise; `W` can be used to toggle this on the fly
        mu4e-headers-include-related 'nil

        ; TODO https://github.com/djcb/mu/issues/1250
        mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum
        mu4e-view-show-addresses t

        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-refile-folder "/Archive"

        mu4e-update-interval 300

        auth-sources '("~/dotfiles-private/mail/authinfo.gpg")
        smtpmail-auth-supported '(login)

        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl

        ;; TODO add a shortcut for mml-secure-message-encrypt-pgpmime
        mml-secure-openpgp-encrypt-to-self t

        ;; fix for mbsync UID problem (nman isync mbsync)
        mu4e-change-filenames-when-moving t

        mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed AND NOT maildir:/Archive" "Unread messages" 117)
         ("date:today..now" "Today's messages (all)" 116)
         ("date:..now AND not flag:trashed AND NOT (maildir:/Archive OR maildir:/Sent)" "Last 7 days (inbox)" 119)
         ("mime:image/*" "Messages with images" 112))

        mu4e-contexts
        `(,(make-mu4e-context
            :name "Mail"
            :enter-func (lambda () (mu4e-message "Entering Mail context"))
            :leave-func (lambda () (mu4e-message "Leaving Mail context"))
            ;;
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg
                             :to "mail@profpatsch.de")))
            :vars '((user-mail-address  . "mail@profpatsch.de")
                    (user-full-name     . "Profpatsch")
                    (mu4e-maildir       . "~/.Mail/mail")
                    (smtpmail-smtp-user . "mail@profpatsch.de")
                    )
            )
          )
        )
)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(beacon-color "#ff9da4")
 '(compilation-message-face (quote default))
 '(create-lockfiles nil)
 '(cua-global-mark-cursor-color nil)
 '(cua-normal-cursor-color nil)
 '(cua-overwrite-cursor-color "")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(dhall-type-check-inactivity-timeout 100000000000000000)
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
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#eee8d5" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-pos-tip-mode t)
 '(flycheck-pos-tip-timeout 0)
 '(foreground-color "#cccccc")
 '(frame-background-mode (quote dark))
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
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-commit-arguments nil)
 '(magit-diff-refine-hunk t)
 '(magit-diff-use-overlays nil)
 '(magit-dispatch-arguments nil)
 '(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))
 '(magit-revision-show-gravatars nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-babel-load-languages (quote ((shell . t) (dot . t))))
 '(org-clock-clocktable-default-properties (quote (:maxlevel 4 :scope file)))
 '(org-confirm-babel-evaluate nil)
 '(org-html-html5-fancy t)
 '(org-html-indent nil)
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
    (transient direnv parent-mode flx reformatter pos-tip nixos-options utop tuareg caml ocp-indent merlin github-search github-clone github-browse-file gist white-sand-theme rebecca-theme org-mime exotica-theme gitignore-mode ghub let-alist epl dhall-mode org-category-capture helm-notmuch notmuch ox-reveal epresent toml-mode racer flycheck-rust seq cargo rust-mode flycheck-elm elm-mode winum madhat2r-theme fuzzy go-guru go-eldoc company-go go-mode psci purescript-mode psc-ide spinner autothemer bind-map haml-mode selectric-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic csv-mode org highlight skewer-mode json-snatcher json-reformat dash-functional tern auctex-latexmk yaml-mode wgrep smex ivy-hydra counsel-projectile counsel swiper ivy insert-shebang hide-comnt undo-tree company-auctex auctex pug-mode f org-projectile org-download intero helm-hoogle git-link evil-ediff dumb-jump request simple-httpd package-build zenburn-theme ws-butler window-numbering web-mode ujelly-theme tao-theme spacemacs-theme spaceline smooth-scrolling shm planet-theme persp-mode page-break-lines orgit organic-green-theme org-repo-todo org-pomodoro alert org-plus-contrib open-junk-file omtose-phellack-theme nix-sandbox neotree naquadah-theme monokai-theme moe-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow leuven-theme less-css-mode js2-refactor js2-mode indent-guide hl-todo hindent help-fns+ helm-themes helm-projectile helm-make projectile helm-descbinds helm-c-yasnippet helm-ag haskell-snippets gruvbox-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate git-messenger expand-region exec-path-from-shell evil-surround evil-search-highlight-persist evil-mc evil-matchit evil-magit evil-iedit-state iedit evil-exchange emmet-mode dracula-theme darktooth-theme company-quickhelp color-theme-sanityinc-tomorrow buffer-move bracketed-paste badwolf-theme auto-yasnippet yasnippet auto-compile apropospriate-theme anti-zenburn-theme ample-theme ace-link ace-jump-helm-line auto-complete avy ghc anzu smartparens haskell-mode flycheck company helm helm-core magit magit-popup git-commit with-editor gh marshal pcache ht hydra s use-package which-key evil dash zonokai-theme zen-and-art-theme web-beautify volatile-highlights vi-tilde-fringe uuidgen underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powerline popwin popup pkg-info phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paredit paradox packed org-present org-bullets oldlace-theme occidental-theme obsidian-theme noctilux-theme nix-mode niflheim-theme mustang-theme multiple-cursors move-text monochrome-theme molokai-theme mmm-mode minimal-theme magit-gh-pulls magit-annex macrostep lush-theme lorem-ipsum logito log4e livid-mode linum-relative link-hint light-soap-theme json-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ ido-vertical-mode hungry-delete htmlize hlint-refactor highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-swoop helm-nixos-options helm-mode-manager helm-gitignore helm-flx helm-css-scss helm-company hc-zenburn-theme gruber-darker-theme goto-chg golden-ratio gnuplot gntp gitconfig-mode gitattributes-mode git-timemachine gh-md gandalf-theme flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-numbers evil-nerd-commenter evil-lisp-state evil-indent-plus evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme elisp-slime-nav django-theme diminish define-word darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-shell company-nixos-options company-ghci company-ghc company-cabal column-enforce-mode colorsarenice-theme color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key auto-highlight-symbol async ample-zen-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(pos-tip-use-relative-coordinates t)
 '(projectile-enable-caching t)
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(racer-rust-src-path nil)
 '(rainbow-identifiers-cie-l*a*b*-lightness 25)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(ring-bell-function (quote ignore))
 '(rust-format-on-save nil)
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
 '(vc-annotate-background-mode nil)
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
 '(default ((t (:background "#232323" :foreground "#E6E1DC" :family "Hasklig" :foundry "ADBO" :slant normal :weight normal :height 128 :width normal)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(ahs-case-fold-search nil t)
 '(ahs-default-range 'ahs-range-whole-buffer t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"])
 '(ansi-term-color-vector
   [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(beacon-color "#ff9da4")
 '(compilation-message-face 'default)
 '(create-lockfiles nil)
 '(cua-global-mark-cursor-color nil)
 '(cua-normal-cursor-color nil)
 '(cua-overwrite-cursor-color "")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(dhall-type-check-inactivity-timeout 100000000000000000)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(diff-hl-flydiff-mode t)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };"))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#eee8d5" t)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-pos-tip-mode t)
 '(flycheck-pos-tip-timeout 0)
 '(foreground-color "#cccccc")
 '(frame-background-mode 'dark)
 '(global-diff-hl-amend-mode t)
 '(global-diff-hl-mode t)
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };") t)
 '(graphviz-dot-view-command "dot -T png %s | feh -")
 '(haskell-indent-spaces 4)
 '(helm-completion-style 'emacs)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(line-move-visual nil)
 '(lsp-enable-snippet nil)
 '(magit-commit-arguments nil)
 '(magit-diff-refine-hunk t)
 '(magit-diff-use-overlays nil)
 '(magit-dispatch-arguments nil)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(magit-revision-show-gravatars nil)
 '(magit-status-sections-hook
   '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpushed-to-pushremote magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream))
 '(nix-indent-function 'indent-relative)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-babel-load-languages '((shell . t) (dot . t)))
 '(org-clock-clocktable-default-properties '(:maxlevel 4 :scope file))
 '(org-confirm-babel-evaluate nil)
 '(org-html-html5-fancy t)
 '(org-html-indent nil)
 '(org-structure-template-alist
   '(("n" "#+NAME: ")
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
     ("I" "#+INCLUDE: %file ?")))
 '(package-selected-packages
   '(frames-only-mode dap-mode bui transient direnv parent-mode flx reformatter pos-tip nixos-options utop tuareg caml ocp-indent merlin github-search github-clone github-browse-file gist white-sand-theme rebecca-theme org-mime exotica-theme gitignore-mode ghub let-alist epl dhall-mode org-category-capture helm-notmuch notmuch ox-reveal epresent toml-mode racer flycheck-rust seq cargo rust-mode flycheck-elm elm-mode winum madhat2r-theme fuzzy go-guru go-eldoc company-go go-mode psci purescript-mode psc-ide spinner autothemer bind-map haml-mode selectric-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode cython-mode company-anaconda anaconda-mode pythonic csv-mode org highlight skewer-mode json-snatcher json-reformat dash-functional tern auctex-latexmk yaml-mode wgrep smex ivy-hydra counsel-projectile counsel swiper ivy insert-shebang hide-comnt undo-tree company-auctex auctex pug-mode f org-projectile org-download intero helm-hoogle git-link evil-ediff dumb-jump request simple-httpd package-build zenburn-theme ws-butler window-numbering web-mode ujelly-theme tao-theme spacemacs-theme spaceline smooth-scrolling shm planet-theme persp-mode page-break-lines orgit organic-green-theme org-repo-todo org-pomodoro alert org-plus-contrib open-junk-file omtose-phellack-theme nix-sandbox neotree naquadah-theme monokai-theme moe-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow leuven-theme less-css-mode js2-refactor js2-mode indent-guide hl-todo hindent help-fns+ helm-themes helm-projectile helm-make projectile helm-descbinds helm-c-yasnippet helm-ag haskell-snippets gruvbox-theme graphviz-dot-mode grandshell-theme gotham-theme google-translate git-messenger expand-region exec-path-from-shell evil-surround evil-search-highlight-persist evil-mc evil-matchit evil-magit evil-iedit-state iedit evil-exchange emmet-mode dracula-theme darktooth-theme company-quickhelp color-theme-sanityinc-tomorrow buffer-move bracketed-paste badwolf-theme auto-yasnippet yasnippet auto-compile apropospriate-theme anti-zenburn-theme ample-theme ace-link ace-jump-helm-line auto-complete avy ghc anzu smartparens haskell-mode flycheck company helm helm-core magit magit-popup git-commit with-editor gh marshal pcache ht hydra s use-package which-key evil dash zonokai-theme zen-and-art-theme web-beautify volatile-highlights vi-tilde-fringe uuidgen underwater-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rainbow-delimiters railscasts-theme quelpa purple-haze-theme professional-theme powerline popwin popup pkg-info phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paredit paradox packed org-present org-bullets oldlace-theme occidental-theme obsidian-theme noctilux-theme nix-mode niflheim-theme mustang-theme multiple-cursors move-text monochrome-theme molokai-theme mmm-mode minimal-theme magit-gh-pulls magit-annex macrostep lush-theme lorem-ipsum logito log4e livid-mode linum-relative link-hint light-soap-theme json-mode js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme info+ ido-vertical-mode hungry-delete htmlize hlint-refactor highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-swoop helm-nixos-options helm-mode-manager helm-gitignore helm-flx helm-css-scss helm-company hc-zenburn-theme gruber-darker-theme goto-chg golden-ratio gnuplot gntp gitconfig-mode gitattributes-mode git-timemachine gh-md gandalf-theme flycheck-pos-tip flycheck-haskell flx-ido flatui-theme flatland-theme fish-mode firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-numbers evil-nerd-commenter evil-lisp-state evil-indent-plus evil-escape evil-args evil-anzu eval-sexp-fu espresso-theme elisp-slime-nav django-theme diminish define-word darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme company-web company-tern company-statistics company-shell company-nixos-options company-ghci company-ghc company-cabal column-enforce-mode colorsarenice-theme color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clean-aindent-mode cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key auto-highlight-symbol async ample-zen-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ac-ispell))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(pos-tip-use-relative-coordinates t)
 '(projectile-enable-caching t)
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(racer-rust-src-path nil t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 25)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(rust-format-on-save nil)
 '(safe-local-variable-values
   '((eval company-mode -42)
     (eval company-mode nil)
     (hindent-style . "chris-done")))
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy scala-indent:operator-strategy)
 '(select-enable-clipboard nil)
 '(send-mail-function 'smtpmail-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sp-autodelete-closing-pair nil)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tooltip-hide-delay 0)
 '(tvl-depot-path "/home/philip/depot")
 '(undo-limit 80000000)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
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
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(ws-butler-global-mode t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#232323" :foreground "#E6E1DC" :family "Hasklig" :foundry "ADBO" :slant normal :weight normal :height 128 :width normal)))))
)

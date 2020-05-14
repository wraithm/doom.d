;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Transparent titlebar
(when IS-MAC
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Window settings
(add-to-list 'default-frame-alist '(width . 180))
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(left . 500))

(defun open-frame-maximized ()
  (interactive)
  (modify-frame-parameters (make-frame) '((fullscreen . maximized))))

(defun open-frame-other-window ()
  (interactive)
  (modify-frame-parameters
   (make-frame)
   '((width . 250)
     (height . 100)
     (top . 250)
     (left + -1000)
     (fullscreen . maximized))))

(map! "s-n" 'make-frame)

;; Transparent background on terminal
(defun unspecified-bg-frame (frame)
  (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame)
      (set-face-background 'linum "unspecified-bg" frame)))
(unspecified-bg-frame (selected-frame))
(add-hook 'after-make-frame-functions 'unspecified-bg-frame)

;; Basics
(setq-default
 tab-width 4
 indent-tabs-mode nil
 fill-column 120
 vc-handled-backends '(Git) ; to disable vc-mode entirely
 )

(add-hook! 'before-save-hook 'delete-trailing-whitespace)
(add-hook! fundamental-mode 'flyspell-mode)
(add-hook! fundamental-mode 'turn-on-auto-fill)

;; evil-mode
(setq! evil-shift-width 4)

(map! "M-;" 'evilnc-comment-or-uncomment-lines)

(map!
 :n "C-w" 'evil-window-map
 :n "C-h" 'evil-window-left
 :n "C-j" 'evil-window-down
 :n "C-k" 'evil-window-up
 :n "C-l" 'evil-window-right
 :m "C-h" 'evil-window-left
 :m "C-j" 'evil-window-down
 :m "C-k" 'evil-window-up
 :m "C-l" 'evil-window-right
 :n "U" 'universal-argument
 :m "U" 'universal-argument
 )

(evil-ex-define-cmd "W" 'save-buffer)
(evil-ex-define-cmd "Q" 'save-buffers-kill-terminal)
(evil-ex-define-cmd "BD" 'kill-this-buffer)

;; Stamp operator
(after! evil-snipe (evil-snipe-mode -1)) ; disable snipe mode

(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  "Delete from beg to end and send to \"_ register"
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))

(evil-define-operator evil-stamp (beg end)
  "Replace text-object with 0th register contents"
  (evil-delete-without-register beg end)
  (evil-paste-from-register ?0))

(map! :n "S" 'evil-stamp)

(evil-set-initial-state 'sql-interactive-mode 'emacs)

;; shell
(evil-set-initial-state 'eshell-mode 'emacs)
(add-hook! term-mode
 (lambda ()
   (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
   (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))
(setq term-buffer-maximum-size 10000)

;; Highlight mode-line instead of audible bell
(defvar ring-bell-mode-line-color "#F2804F")
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line ring-bell-mode-line-color)
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; emacsclient open multiple files in separate windows
(defvar server-visit-files-custom-find:buffer-count)
(defadvice server-visit-files
  (around server-visit-files-custom-find
      activate compile)
  "Maintain a counter of visited files from a single client call."
  (let ((server-visit-files-custom-find:buffer-count 0))
    ad-do-it))
(defun server-visit-hook-custom-find ()
  "Arrange to visit the files from a client call in separate windows."
  (if (zerop server-visit-files-custom-find:buffer-count)
      (progn
    (delete-other-windows)
    (switch-to-buffer (current-buffer)))
    (let ((buffer (current-buffer))
      (window (split-window-sensibly)))
      (switch-to-buffer buffer)
      (balance-windows)))
  (setq server-visit-files-custom-find:buffer-count
    (1+ server-visit-files-custom-find:buffer-count)))
(add-hook 'server-visit-hook 'server-visit-hook-custom-find)

;; Mac specific stuff
(map! "M-C-f" 'toggle-frame-fullscreen)
(when (eq system-type 'darwin)
  (advice-add 'ns-new-frame :after '(scroll-bar-mode -1))
  (advice-add 'ns-new-frame :after #'toggle-frame-fullscreen))

;; ediff
(require 'ediff)
(defun my-kill-ediff-buffers ()
  (kill-buffer ediff-buffer-A)
  (kill-buffer ediff-buffer-B)
  (kill-buffer ediff-buffer-C))
(add-hook 'ediff-quit-hook 'my-kill-ediff-buffers)

(require 'org-table)
(defun md-table-align ()
  (interactive)
  (org-table-align)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

;; web-mode
(after! web
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  )

;; tramp
(after! tramp
  (setq! tramp-default-method "sshx")
  )

;; elisp
(add-hook! emacs-lisp-mode 'turn-off-smartparens-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; LSP
(setq
 lsp-ui-sideline-enable nil
 ;; lsp-ui-doc-enable t
 lsp-ui-doc-max-height 30
 lsp-ui-doc-max-width 100
 )

;; Haskell
(after! haskell
(setq! haskell-stylish-on-save t
       haskell-interactive-set-+c t
       haskell-indentation-layout-offset 4
       haskell-indentation-left-offset 4
       haskell-compile-cabal-build-command "stack build --test --bench --no-run-tests --no-run-benchmarks --no-interleaved-output"
       haskell-compile-cabal-build-alt-command (concat "stack clean && " haskell-compile-cabal-build-command)
       haskell-process-type 'stack-ghci
       haskell-process-suggest-remove-import-lines t
       haskell-process-suggest-hoogle-imports t
       haskell-process-auto-import-loaded-modules t
       haskell-process-log t
       )

;; TODO somehow add haskell dash docsets

(defun stack-compile-command ()
  (interactive)
  (setq compile-command haskell-compile-cabal-build-command))

(defun haskell-company-backends ()
  (set (make-local-variable 'company-backends)
                   (append '((company-lsp company-capf company-dabbrev-code))
                           company-backends)))

(add-hook! haskell-mode 'stack-compile-command)
(add-hook! haskell-mode 'haskell-company-backends)
;; TODO is yas activated?
;; TODO lsp-enable-xref?

(evil-define-motion my-haskell-navigate-imports ()
  "Navigate imports with evil motion"
  :jump t
  :type line
  (haskell-navigate-imports))

  (map!
   :map haskell-mode-map
   :localleader
   "t" 'haskell-mode-show-type-at
   "h" 'hoogle
   "i" 'my-haskell-navigate-imports
   "r" 'haskell-process-restart
   "q" 'lsp-ui-flycheck-list
   "c" 'haskell-compile
   "f" 'first-error
   "n" 'next-error
   "p" 'previous-error
   "F" 'haskell-goto-first-error
   "N" 'haskell-goto-next-error
   "P" 'haskell-goto-previous-error
   "a" 'align
   )

(after! lsp-haskell
  (setq warning-minimum-level ':error) ; This is temporary for a bug in lsp-ui that pops up errors
  (lsp-haskell-set-hlint-on)
  (lsp-haskell-set-liquid-off)
  )

(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-types
                  (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-assignment
                  (regexp . "\\(\\s-+\\)=\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-arrows
                  (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))
(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(haskell-left-arrows
                  (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                  (modes quote (haskell-mode literate-haskell-mode)))))

(after! projectile
  (projectile-register-project-type 'haskell-stack '("stack.yaml")
    :compile haskell-compile-cabal-build-command
    :test "stack build --test"))
)

;; TODO dash-at-point
;; TODO remove autoparens
;; TODO alignment key
;; TODO org-mode setup
;; TODO ix
;; TODO irc
;; TODO magit, forge, github-review

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq
 user-full-name "Matthew Wraith"
 user-mail-address "matt@bitnomial.com"
 )

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font (font-spec :family "SF Mono" :size 12)
 doom-big-font (font-spec :family "SF Mono" :size 16)
 doom-variable-pitch-font (font-spec :family "Avenir Next" :size 12)
 )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(defun my-paragraph-hook ()
  (setq paragraph-start "\\|[    ]*$"
        paragraph-separate "[     ]*$"))
(add-hook! org-mode #'my-paragraph-hook)

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(after! org
  (map!
   :map org-mode-map
   :localleader
   "A" 'mark-done-and-archive
   ))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

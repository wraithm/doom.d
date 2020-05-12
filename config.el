;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Code:

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthew Wraith"
      user-mail-address "matt@bitnomial.com")

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
 doom-font (font-spec :family "SF Mono" :size 14)
 doom-big-font (font-spec :family "SF Mono" :size 20)
 doom-variable-pitch-font (font-spec :family "Avenir Next" :size 14)
 )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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


;; evil-mode
(setq evil-shift-width 4
      evil-want-C-w-in-emacs-state t)

(map!
 :n "C-w" 'evil-window-map
 :n "C-w C-h" 'undefined
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

(use-package evil-nerd-commenter
  :config
  (map! "M-;" 'evilnc-comment-or-uncomment-lines)
  (map! :n "gc" 'evilnc-comment-operator)
  )
(use-package evil-nerd-commenter-operator)

(evil-ex-define-cmd "W" 'save-buffer)
(evil-ex-define-cmd "Q" 'save-buffers-kill-terminal)
(evil-ex-define-cmd "BD" 'kill-this-buffer)

;; Stamp operator
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

(use-package evil-indent-textobject)

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

(setq-default
 tab-width 4
 indent-tabs-mode nil
 fill-column 120
 )

(setq vc-handled-backends '(Git)) ; to disable vc-mode entirely

(menu-bar-mode -1)
(setq menu-bar-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun no-menubar-frame (&optional frame)
  "Do not display the menubar in FRAME (default: selected frame)."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines 0))
(add-hook 'after-make-frame-functions 'no-menubar-frame)

;; Mac specific stuff
(map! "M-C-f" 'toggle-frame-fullscreen)
(when (eq system-type 'darwin)
  (advice-add 'ns-new-frame :after '(scroll-bar-mode -1))
  (advice-add 'ns-new-frame :after #'toggle-frame-fullscreen))

(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'turn-on-auto-fill)


(after! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

;; ediff
(require 'ediff)
(defun my-kill-ediff-buffers ()
  (kill-buffer ediff-buffer-A)
  (kill-buffer ediff-buffer-B)
  (kill-buffer ediff-buffer-C))
(add-hook 'ediff-quit-hook 'my-kill-ediff-buffers)


;; Compilation
;; (require 'compile)
;; (setq compilation-scroll-output t)
;; (add-to-list 'display-buffer-alist
;;              '("\\*compilation\\*"
;;                ;; (window-height . 25)
;;                ;; display-buffer-pop-up-window
;;                ;; display-buffer-at-bottom

;;                display-buffer-reuse-window
;;                (reusable-frames . t)
;;                (inhibit-switch-frame . t)
;;                ))


(setq-default markdown-command "pandoc -f gfm")

(require 'org-table)
(defun md-table-align ()
  (interactive)
  (org-table-align)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

(map! :n "C-p" 'projectile-find-file)

;; rcirc
(require 'rcirc)
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
(defun rcirc-detach-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
           (eq (process-status (rcirc-buffer-process)) 'open))
      (with-rcirc-server-buffer
    (setq rcirc-buffer-alist
          (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
      (rcirc-send-string (rcirc-buffer-process)
                 (concat "DETACH " rcirc-target))))
    (setq rcirc-target nil)
    (kill-buffer buffer)))
(define-key rcirc-mode-map (kbd "C-c C-d") 'rcirc-detach-buffer)

;; (load "~/.emacs.d/irc.el")

;; (load "~/.emacs.d/wolframalpha.el")

;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(require 'tramp)
(setq tramp-default-method "sshx")

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

;; Transparent titlebar
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Transparent background on terminal
(defun unspecified-bg-frame (frame)
  (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame)
      (set-face-background 'linum "unspecified-bg" frame)))
(unspecified-bg-frame (selected-frame))
(add-hook 'after-make-frame-functions 'unspecified-bg-frame)

;; Haskell
(defun stack-compile-command ()
  (interactive)
  (setq compile-command "stack build --test --bench --no-run-tests --no-run-benchmarks --no-interleaved-output"))


(after! haskell-mode
  (setq haskell-stylish-on-save t
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

  (evil-define-motion my-haskell-navigate-imports ()
    "Navigate imports with evil motion"
    :jump t
    :type line
    (haskell-navigate-imports))

  (map! :map haskell-mode-map
        :localleader
        "t" 'haskell-mode-show-type-at
        "h" 'hoogle
        "i" 'my-haskell-navigate-imports
        "r" 'haskell-process-restart
        "q" 'lsp-ui-flycheck-list
        )

  ;; (setq warning-minimum-level ':error) ; This is temporary for a bug in lsp-ui that pops up errors
 (lsp-haskell-set-hlint-on)
 (lsp-haskell-set-liquid-off)

  ;; (evil-leader/set-key-for-mode 'haskell-mode "h" 'hoogle)
  ;; (evil-leader/set-key-for-mode 'haskell-mode "i" 'my-haskell-navigate-imports)
  ;; (evil-leader/set-key-for-mode 'haskell-mode "t" 'haskell-mode-show-type-at)
  ;; (evil-leader/set-key-for-mode 'haskell-mode "r" 'haskell-process-restart)

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

  )

(projectile-register-project-type 'haskell-stack '("stack.yaml")
    :compile haskell-compile-cabal-build-command
    :test "stack build --test")

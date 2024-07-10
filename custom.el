(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((sql-server . "localhost")
     (sql-database . "main")
     (sql-postgres-login-params
      (user :default "app")
      (server :default "localhost")
      (database :default "main"))
     (sql-postgres-options "-Uapp" "-P" "pager=off")
     (haskell-hoogle-url . "http://localhost:8123/?hoogle=%s")
     (haskell-hoogle-command . "stack hoogle --")
     (haskell-hoogle-server-command . (lambda (port)
                                        (list "stack" "hoogle" "--" "server" "--local"
                                              "-p" (number-to-string port))))
     (ormolu-process-path . "fourmolu")
     (lsp-haskell-formatting-provider . "fourmolu")
     (haskell-stylish-on-save)
     (haskell-process-type . stack-ghci)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-layout-offset . 4)
     (web-mode-code-indent-offset . 2)
     (web-mode-css-indent-offset . 2)
     (web-mode-markup-indent-offset . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

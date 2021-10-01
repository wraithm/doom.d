(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default))
 '(safe-local-variable-values
   '((web-mode-code-indent-offset . 2)
     (web-mode-css-indent-offset . 2)
     (web-mode-markup-indent-offset . 2)
     (ormolu-process-path . "fourmolu")
     (lsp-haskell-formatting-provider . "fourmolu")
     (haskell-stylish-on-save)
     (haskell-process-type . stack-ghci)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-hoogle-url . "http://localhost:8123/?hoogle=%s")
     (haskell-hoogle-command . "stack hoogle --")
     (haskell-hoogle-server-command lambda
                                    (port)
                                    (list "stack" "hoogle" "--" "server" "--local" "-p"
                                          (number-to-string port)))
     (haskell-indentation-layout-offset . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

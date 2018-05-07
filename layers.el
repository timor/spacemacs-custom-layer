(configuration-layer/remove-layer 'helm)
(configuration-layer/declare-layers '((ivy :variables ivy-initial-inputs-alist nil)
                                      (shell :variables shell-default-shell 'multi-term)
                                      ))

(configuration-layer/remove-layer 'helm)
(configuration-layer/declare-layers '(
                                      (ivy :variables
                                           ivy-initial-inputs-alist nil
                                           ivy-wrap t
                                           )
                                      (shell :variables shell-default-shell 'multi-term)
                                      ))

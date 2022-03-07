# An emacs mode for quarto

`Quarto-mode` is an emacs mode for editing [quarto](https://quarto.org) documents.

## Installing

`quarto-mode` is available on MELPA. From emacs,

```
M-x refresh-package-contents
M-x install-package
  quarto-mode
```

## quarto-mode + ESS

If you have [ESS](https://ess.r-project.org/), `quarto-mode` will use it. Otherwise, it won't. Specifically, `quarto-mode` does not depend on ESS (or R), which means that if you want to use those features, you have to install ESS separately.

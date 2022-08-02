# An emacs mode for quarto

`Quarto-mode` is an emacs mode for editing [quarto](https://quarto.org) documents.

## Installing

`quarto-mode` is available on MELPA. From emacs,

```
M-x package-refresh-contents
M-x package-install
  quarto-mode
```

### Using `quarto-mode`

Add this to your `.emacs` or `~/.emacs.d/init.el` file:

```elisp
;; load the library
(require 'quarto-mode)
```

The `quarto-mode` package will associate a quarto [polymode](https://github.com/polymode/polymode) to `.qmd` files.
That means that there isn't an actual `quarto-mode` mode. 
If you want to associate other files to the quarto polymode, you should use `poly-quarto-mode`, such as:

```elisp
;; Note that the following is not necessary to run quarto-mode in .qmd files! It's merely illustrating
;; how to associate different extensions to the mode.
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-quarto-mode))

;; Or, with use-package:
(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode))
  )
```

### Dependencies

Quarto-mode requires the following packages to be installed:

- `(polymode "0.2.2")`
- `(poly-markdown "0.2.2")`
- `(markdown-mode "2.3")`
- `(request "0.3.2")`

### quarto-mode + ESS

If you have [ESS](https://ess.r-project.org/), `quarto-mode` will use it. Otherwise, it won't. Specifically, `quarto-mode` does not depend on ESS (or R), which means that if you want to use those features, you have to install ESS separately.

# Features

- `M-x quarto-preview`. Start a `quarto preview` server that watches quarto content for changes and automatically refreshes it. If the current buffer has an associated file that exists in a quarto project, the command will preview the entire project. Otherwise, it will preview the specific file.
- Integration with poly-markdown's compilation. The default poly-markdown configuration runs plain `pandoc` on the document; `quarto-mode` uses `quarto render`.

## Differences in behavior between quarto-mode and markdown-mode

* `C-c C-c *` behavior

  `quarto-mode` uses features specific to quarto that make it behave
  differently from what `markdown-mode` users might expect. By
  default, `quarto-mode` uses `quarto preview`, which works through a
  custom web server, and does not produce disk output upon
  preview. `quarto preview` is significantly faster than rerendering
  entire files in interactive mode, so we encourage you to use it.
  
  However, if you wish to not use `quarto preview` and instead depend
  on the typical rendering mode of previewing, you can restore the
  standard `markdown-mode` behavior by changing the
  `quarto-force-preview` customization variable.

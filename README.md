# An emacs mode for quarto

`Quarto-mode` is an emacs mode for editing [quarto](https://quarto.org) documents.

## Installing

Copy `quarto-mode.el` to where you keep your `.el` files (typically `~/.emacs.d`) and add

    (require 'quarto-mode)

to your emacs configuration file.

### Dependencies

Quarto-mode requires the following packages to be installed:

- `(polymode "0.2.2")`
- `(poly-markdown "0.2.2")`
- `(markdown-mode "2.3")`
- `(request "0.3.2")`

### quarto-mode + ESS

If you have [ESS](https://ess.r-project.org/), `quarto-mode` will use it. Otherwise, it won't. Specifically, `quarto-mode` does not depend on ESS (or R), which means that if you want to use those features, you have to install ESS separately.

### MELPA

Although `quarto-mode` is on melpa, there is currently an issue that prevents `quarto-mode.el` from working on MELPA. We are investigating and will provide a fix as soon as we're confident.

# Features

- `M-x quarto-preview`. Start a `quarto preview` server that watches quarto content for changes and automatically refreshes it. If the current buffer has an associated file that exists in a quarto project, the command will preview the entire project. Otherwise, it will preview the specific file.
- Integration with poly-markdown's compilation. The default poly-markdown configuration runs plain `pandoc` on the document; `quarto-mode` uses `quarto render`.


## Emacs modes for quarto

We are currently in the process of [getting MELPA approval for the
`quarto-mode.el`
package](https://github.com/melpa/melpa/pull/7900). In the meantime,
there are two options for using Quarto with Emacs:

### Option 1: `quarto-mode.el` (extending `poly-markdown-mode` and/or `poly-markdown+r-mode`)

If you're comfortable adding packages manually to your Emacs setup,
download `quarto-mode.el` from this repo, add it to your path, and add
`(require 'quarto-mode)`.

You will need to install either `poly-markdown` or `poly-R` alongside `quarto-mode`
(use `poly-markdown` for Jupyter/Python, use `poly-R` to use ESS).

`quarto-mode` supports `quarto preview`, which watches your filesystem 
for changes to quarto files and shows a preview on a web browser window. You can
start a `quarto preview` session through the Quarto menu on the Emacs
menu bar or directly by calling the function `quarto-preview`.

If you run `quarto-preview` on a `.qmd` buffer, `quarto preview` will rerender only that file. If you run `quarto-preview` on a `_quarto.yml` buffer, it will watch all files in that project.

### Option 2: `poly-markdown-mode` and/or `poly-markdown+r-mode` mapped to `.qmd` files

If you don't wish to install `quarto-mode.el`, you can use [`poly-markdown` or `poly-R`
modes](https://polymode.github.io/), both available in the standard
Emacs package repositories. In this configuration there are no special 
quarto commands (e.g. `quarto-preview`) but you'll still get all polymode functionality in the quarto code cells and YAML front matter.

To ensure Emacs associates the right mode with `.qmd` files when
opening them, add the following to your `.emacs`:

    ;; if you're using poly-R
    (add-to-list 'auto-mode-alist '("\\.qmd$" . poly-markdown+r-mode))
    ;; if you're using poly-markdown
    (add-to-list 'auto-mode-alist '("\\.qmd$" . poly-markdown-mode))


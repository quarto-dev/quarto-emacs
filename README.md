# Emacs modes for quarto

We are currently in the process of [getting MELPA approval for the
`quarto-mode.el`
package](https://github.com/melpa/melpa/pull/7900). In the meantime,
there are a few options below:

## Using quarto with `quarto-mode.el`

If you're comfortable adding packages manually to your Emacs setup,
download `quarto-mode.el` from this repo, add it to your path, and add
`(require 'quarto-mode)`. You will need the same dependencies as
above. `quarto-mode` automatically associates itself to `.qmd` files.

`quarto-mode` builds on `poly-R` and `poly-markdown` and supports
`quarto preview` mode, which watches your filesystem for changes to
quarto files and shows a preview on a web browser window. You can
start a `quarto preview` session through the Quarto menu on the Emacs
menu bar or directly by calling the function `quarto-preview`.

## Using Quarto with existing Emacs packages

Quarto files work well in [`poly-markdown` or `poly-R`
modes](https://polymode.github.io/), both available in the standard
Emacs package repositories. `poly-markdown` includes full language
support for any mode you have installed in Emacs, and it uses the
chunk name to decide.

    ```{python}
    # this will exist under python-mode
    ```
    
    ```{julia}
    # this will exist under julia-mode (which you'll have to install separately)
    ```

If you want to use [ESS](https://ess.r-project.org/) for your R code
chunks, you'll need `poly-R`. Otherwise, `poly-markdown` is enough.

### Installation

To install either mode:

    M-x list-packages
    
Then, search for `poly-markdown` or `poly-R`, hit Enter, and finally
press the `Install` button on the subsequent window, under the
"Status" line.

### Enabling the modes

To ensure Emacs associates the right mode with `.qmd` files when
opening them, add the following to your `.emacs`:

    ;; if you're using poly-R
    (add-to-list 'auto-mode-alist '("\\.qmd$" . poly-markdown+r-mode))
    ;; if you're using poly-markdown
    (add-to-list 'auto-mode-alist '("\\.qmd$" . poly-markdown-mode))


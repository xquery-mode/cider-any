# Cider Any

Evaluate anything in cider.

# XQuery

## Installation

System requirements:

* Clojure
* Leiningen
* Cider
* Page break lines emacs lisp library
* Uruk clojure library
* MarkLogic server access

Load Emacs Lisp libraries in your configuration.

```lisp
(require 'cider-any)
(require 'cider-any-uruk)
```

Enable cider-any in xquery-mode by default.

```lisp
(add-hook 'xquery-mode-hook 'cider-any-mode)
```

Grain access to the MarkLogic account.

```lisp
(setq cider-any-uruk-uri "xdbc://localhost:8889/"
      cider-any-uruk-user "proofit404"
      cider-any-uruk-password "<secret>")
```

## Usage

Start cider repl in the leiningen project.  Uruk library must be
pinned in the project.clj in the dependencies section.  When you hit
`C-c C-c` XQuery buffer will be evaluated on MarkLogic server you
grain access earlier.  Result of this evaluation will be displayed in
the buffer with corresponding major mode.

If you want to open result document in the web browser you can
customize this behavior.

```lisp
(setq cider-any-uruk-handler 'cider-any-uruk-browse)
```

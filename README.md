[![Build Status](https://travis-ci.com/twlz0ne/meal.el.svg?branch=master)](https://travis-ci.com/twlz0ne/meal.el)

# meal.el

Multi-file support for `eval-after-load`.

## Requirements

- Emacs 25.1+

## Installation

Clone this repository and add the following to your `.emacs`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/meal"))
(require 'meal)
```

## Usage

```elisp
(with-eval-after-load '(and a (or b c))
  ...)
```

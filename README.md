Impatient Mode
==============

See the effect of your HTML as you type it.

This package requires both _simple-httpd_ and _htmlize_ in order to
operate. The _simple-httpd_ webserver runs within emacs to serve up
your buffers as you edit them. _htmlize_ is used to send font lock
highlighting to clients for non-HTML buffers.

_simple-httpd_ can be installed through MELPA or directly from GitHub.

 * http://melpa.milkbox.net/
 * https://github.com/skeeto/emacs-http-server

Add the library to your load path and load it:

```el
(add-to-list 'load-path "~/.emacs.d/impatient-mode")
(require 'impatient-mode)
```

Also, make sure you enable _simple-httpd_'s optional servlet support

```el
(require 'simple-httpd)
(setq httpd-servlets t)
```

Publish buffers by enabling the minor mode `impatient-mode`.

```
M-x impatient-mode
```

And then point your browser to http://localhost:8080/imp/, select a
buffer, and watch your changes appear as you type!

Except for `html-mode` buffers, buffer contents will be run through
`htmlize` before sending to clients. This can be toggled at any time
with `imp-toggle-htmlize`.

```
M-x imp-toggle-htmlize
```

Impatient Mode
==============

See the effect of your HTML as you type it.

 * [YouTube demo](http://youtu.be/QV6XVyXjBO8)

Installation through MELPA
--------------------------

The easiest way to get up and running with _impatient-mode_ is to
install it through [MELPA](http://melpa.milkbox.net/). If you're not
already using MELPA,
[it's quite easy to setup.](http://melpa.milkbox.net/#installing)

Installation from Source
------------------------

If you are installing from source, please note that this package
requires both _simple-httpd_ and _htmlize_ in order to operate. The
_simple-httpd_ webserver runs within emacs to serve up your buffers as
you edit them. _htmlize_ is used to send font lock highlighting to
clients for non-HTML buffers.

_simple-httpd_ can be installed through MELPA or directly from GitHub.

 * http://melpa.milkbox.net/
 * https://github.com/skeeto/emacs-http-server

_htmlize_ is also available through MELPA.

Once you have installed _simple-httpd_ and _htmlize_ and you've cloned
_impatient-mode_, you can add _impatient-mode_ to your load path and
require it:

```el
(add-to-list 'load-path "~/.emacs.d/impatient-mode")
(require 'impatient-mode)
```

Using _impatient-mode_
----------------------

Enable the web server provided by _simple-httpd_:

```el
M-x httpd-start
```

Publish buffers by enabling the minor mode `impatient-mode`.

```
M-x impatient-mode
```

And then point your browser to http://localhost:8080/imp/, select a
buffer, and watch your changes appear as you type!

If you are editing HTML that references resources in other files (like
CSS) you can enable impatient-mode on those buffers as well. This will
cause your browser to live refresh the page when you edit a referenced
resources.

Except for `html-mode` buffers, buffer contents will be run through
`htmlize` before sending to clients. This can be toggled at any time
with `imp-toggle-htmlize`.

```
M-x imp-toggle-htmlize
```

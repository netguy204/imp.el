Impatient Mode
==============

See the effect of your HTML as you type it.

This mode uses the _simple-httpd_ webserver that runs within emacs to
serve up your HTML buffers as you edit them.

_simple-httpd_ can be installed through MELPA
http://melpa.milkbox.net/ or directly from GitHub
https://github.com/skeeto/emacs-http-server

Add the library to your load path and load it:

``` elisp
(add-to-list 'load-path "~/.emacs.d/imp")
(require 'imp)
```

Set the buffer you want to live edit with:

M-x imp-set-current-buffer

And then point your browser to http://localhost:8080/imp-shim and
watch your changes appear as you type!

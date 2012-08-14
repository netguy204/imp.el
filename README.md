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

Also, make sure you enable _simple-httpd_'s optional servlet support

``` elisp
(require 'simple-httpd)
(setq httpd-servlets t)
```

Set the buffer you want to live edit with:

```
M-x imp-set-current-buffer
```

If you'd rather see the contents of your buffer colorized in your browser (instead of being rendered), set the buffer you want with:

```
M-x imp-set-current-buffer-htmlize
```

And then point your browser to http://localhost:8080/imp-shim and
watch your changes appear as you type!

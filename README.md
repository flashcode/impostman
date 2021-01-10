# Import of Postman collections in Emacs

[![Build Status](https://github.com/flashcode/impostman/workflows/CI/badge.svg)](https://github.com/flashcode/impostman/actions?query=workflow%3A%22CI%22)

Postman collections can be imported and used with these Emacs HTTP clients:

- [verb](https://github.com/federicotdn/verb)
- [restclient.el](https://github.com/pashky/restclient.el)

You can use your own functions to write other formats (see [Add new output](#add-new-output).

## Installation

This package requires:

- Emacs ≥ **27.1** (it uses the native support for JSON introduced in Emacs 27)
- [verb](https://github.com/federicotdn/verb) or [restclient](https://github.com/pashky/restclient.el)

You can deploy impostman.el into your site-lisp as usual, then add this line to your Emacs initialization file:

```elisp
(require 'impostman)
```

## Usage

Two functions can be called interactively to import a Postman collection:

- <kbd>M-x</kbd> `impostman-import-file` <kbd>RET</kbd>
- <kbd>M-x</kbd> `impostman-import-string` <kbd>RET</kbd>

The function `impostman-import-file` takes two optional parameters (they are asked if not provided):

- `filename` (optional): the Postman collection
- `output` (optional): the output type (`verb` or `restclient`)

Example:

```elisp
(impostman-import-file "/path/to/collection.json" "verb")
```

The function `impostman-import-string` takes two parameters (the second is optional and asked if not provided):

- `string`: the string with the collection (JSON format)
- `output` (optional): the output type (`verb` or `restclient`)

Example:

```elisp
(impostman-import-string "{}" "verb")
```

The result is displayed in a new buffer with the Emacs HTTP client, and the mode is set to:

- verb: `org-mode` (major) and `verb-mode` (minor)
- restclient: `restclient-mode`

## Customization

Some options can be customized to alter the output, you can list and change them with:
<kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `impostman` <kbd>RET</kbd>.

## Add new output

Two low-level functions can also be called (non interactively), with a custom output (alist).

This alist must be defined like this, for example if your output is for walkman (another HTTP client for Emacs):

```elisp
(defconst my-impostman-walkman-alist
  '((init . my-impostman-walkman-init)
    (header . my-imimpostman-walkman-header)
    (item . my-impostman-walkman-item)
    (request . my-impostman-walkman-request)
    (footer . my-impostman-walkman-footer)
    (end . my-impostman-walkman-end))
  "Emacs walkman output")
```

Keys are fixed symbols and values are [callback functions](#callback-functions).

A function can be `ignore`, in this case it is simply ignored, for example if you don't have anything to do for `init` and `end`:

```elisp
(defconst my-impostman-walkman-alist
  '((init . ignore)
    (header . my-impostman-walkman-header)
    (item . my-impostman-walkman-item)
    (request . my-impostman-walkman-request)
    (footer . my-impostman-walkman-footer)
    (end . ignore))
  "Emacs walkman output")
```

Then you can call for a file:

```elisp
(impostman-parse-file "/path/to/collection.json" my-impostman-walkman-alist)
```

And for a string:

```elisp
(impostman-parse-string "{}" my-impostman-walkman-alist)
```

You can also add your output to the list of impostman outputs, so you can use it with `impostman-import-file` and `impostman-import-string`:

```elisp
(push '("walkman" . my-impostman-walkman-alist) impostman-outputs-alist)
```

This will put your output at the beginning of the alist, so it will be the default output.

#### Callback functions

```elisp
(defun xxx-init ()
  (...))
```

Function called when the output buffer is created and before parsing the collection.

```elisp
(defun xxx-header (name description)
  (...))
```

Function called after `init` and before parsing the collection. It must return a string which is inserted in the output buffer.

Arguments:

- `name` (string): collection name (`unknown` if not found)
- `description` (string): collection description

```elisp
(defun xxx-item (level name description)
  (...))
```

Function called for each item read (a folder in Postman). It must return a string which is inserted in the output buffer.

Arguments:

- `level` (integer): folder level (≥ 2)
- `name` (string): item name
- `description` (string): item description

```elisp
(defun xxx-request (description method url headers body)
  (...))
```

Function called for each request read. It must return a string which is inserted in the output buffer.

Arguments:

- `description` (string): request description
- `method` (string): the HTTP method (`GET`, `POST`, `PUT`, …)
- `url` (string): request URL
- `headers` (alist): request headers
- `body` (string): request body

```elisp
(defun xxx-footer (name)
  (...))
```

Function called at the end of parsing. It must return a string which is inserted in the output buffer.

Arguments:

- `name` (string): collection name (`unknown` if not found)

```elisp
(defun xxx-end ()
  (...))
```

Function called at the end.

## Known limitations

For now the package offers a basic support of Postman collections, the following features are partially implemented:

- autentication: only `basic` and `apikey` are supported
- body: only `raw` is supported.

Pull requests are welcome to add missing features.

## Copyright

Copyright © 2020-2021 [Sébastien Helleu](https://github.com/flashcode)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

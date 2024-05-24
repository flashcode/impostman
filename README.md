# Import of Postman collections in Emacs

[![Build Status](https://github.com/flashcode/impostman/workflows/CI/badge.svg)](https://github.com/flashcode/impostman/actions?query=workflow%3A%22CI%22)
[![MELPA](https://melpa.org/packages/impostman-badge.svg)](https://melpa.org/#/impostman)
[![MELPA Stable](https://stable.melpa.org/packages/impostman-badge.svg)](https://stable.melpa.org/#/impostman)

Postman collections and environments can be imported and used with these Emacs HTTP clients:

- [verb](https://github.com/federicotdn/verb)
- [restclient](https://github.com/pashky/restclient.el)
- other clients with your own functions, see [Add new output](#add-new-output).

## Requirements

This package requires:

- Emacs ≥ 27.1 (it uses the native support for JSON introduced in Emacs 27).

Optional dependencies:

- [verb](https://github.com/federicotdn/verb)
- [restclient](https://github.com/pashky/restclient.el).

Note: without these optional dependencies, you can still convert to all formats, but the mode will not be set in the buffer once converted.

## Installation

You can install Impostman with `package-install` command, either from [MELPA](https://melpa.org/) or [MELPA Stable](https://stable.melpa.org/):

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `impostman` <kbd>RET</kbd>

Alternatively, you can deploy impostman.el into your site-lisp as usual, then add this line to your Emacs initialization file:

```elisp
(require 'impostman)
```

## Usage

Two functions can be called interactively to import a Postman collection, with an optional environment:

- <kbd>M-x</kbd> `impostman-import-file` <kbd>RET</kbd>
- <kbd>M-x</kbd> `impostman-import-string` <kbd>RET</kbd>

The function `impostman-import-file` takes three optional parameters (they are asked interactively if not provided):

- `collection` (optional): the Postman collection
- `environment` (optional): the Postman environment (must be given if variables are used in the collection, can be empty string if the collection does not use any variable from an environment)
- `output` (optional): the output type: `verb`, `restclient` or your custom output.

Example:

```elisp
(impostman-import-file "/path/to/collection.json" "/path/to/environment.json" "verb")
```

The function `impostman-import-string` takes three parameters (the third is optional and asked interactively if not provided):

- `collection`: the string with the collection (JSON format)
- `environment`: the string with the environment (JSON format) (must be given if variables are used in the collection, can be empty string if the collection does not use any variable from an environment)
- `output` (optional): the output type (`verb` or `restclient`).

Example:

```elisp
(impostman-import-string "{}" "" "verb")
```

The result is displayed in a new buffer with the Emacs HTTP client, and the mode is set to:

- verb: `org-mode` (major) and `verb-mode` (minor)
- restclient: `restclient-mode`.

## Customization

Some options can be customized to alter the output, you can list and change them with:

<kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `impostman` <kbd>RET</kbd>

List of variables:

- `impostman-auth-basic-as-elisp-code` (boolean, default: `t`): convert Basic authentication header to elisp code so that the username and password can be easily edited; if set to `nil`, the header is written in Base64
- `impostman-use-variables` (boolean, default: `t`): keep Postman variables in the output and define variables according to the
output; if set to `nil`, no variables are used, they are directly replaced by their values during the import of collection
- `impostman-outputs-alist` (alist, default keys: `verb` and `restclient`): list of outputs, see [Add new output](#add-new-output).

## Add new output

Two low-level functions can also be called (non interactively), with a custom output (alist).

This alist must be defined like this, for example if your output is for walkman (another HTTP client for Emacs):

```elisp
(defconst my-impostman-walkman-alist
  '((init . my-impostman-walkman-init)
    (replace-vars . my-impostman-walkman-replace-vars)
    (header . my-impostman-walkman-header)
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
    (replace-vars . my-impostman-walkman-replace-vars)
    (header . my-impostman-walkman-header)
    (item . my-impostman-walkman-item)
    (request . my-impostman-walkman-request)
    (footer . my-impostman-walkman-footer)
    (end . ignore))
  "Emacs walkman output")
```

Then you can call for a file:

```elisp
(impostman-parse-file "/path/to/collection.json" "/path/to/environment.json" my-impostman-walkman-alist)
```

And for a string:

```elisp
(impostman-parse-string "{}" "" my-impostman-walkman-alist)
```

You can also add your output to the list of impostman outputs, so you can use it with `impostman-import-file` and `impostman-import-string`:

```elisp
(push '("walkman" . my-impostman-walkman-alist) impostman-outputs-alist)
```

This will put your output at the beginning of the alist, so it will be the default output.

### Callback functions

#### init

```elisp
(defun xxx-init (variables)
  (...))
```

Function called when the output buffer is created and before parsing the collection.

Arguments:

- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### replace-vars

```elisp
(defun xxx-replace-vars (string variables)
  (...))
```

Function called to replace Postman variables (format: `{{variable}}`) in a string. It must return a string where the Postman variables have been replaced by the appropriate value (according to the output).

Note: according to the option `impostman-use-variables`, a variable is either replaced with a reference (by default and the format depends on the output) or directly with its value, if the option is set to nil.

Arguments:

- `string` (string): any string
- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### header

```elisp
(defun xxx-header (name description variables)
  (...))
```

Function called after `init` and before parsing the collection. It must return a string which is inserted in the output buffer.

Arguments:

- `name` (string): collection name (`unknown` if not found)
- `description` (string): collection description
- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### item

```elisp
(defun xxx-item (level name description variables)
  (...))
```

Function called for each item read (a folder in Postman). It must return a string which is inserted in the output buffer.

Arguments:

- `level` (integer): folder level (≥ 2)
- `name` (string): item name
- `description` (string): item description
- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### request

```elisp
(defun xxx-request (description method url headers body variables)
  (...))
```

Function called for each request read. It must return a string which is inserted in the output buffer.

Arguments:

- `description` (string): request description
- `method` (string): the HTTP method (`GET`, `POST`, `PUT`, …)
- `url` (string): request URL
- `headers` (alist): request headers (in reverse order: latest header read is the first one in alist)
- `body` (string): request body
- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### footer

```elisp
(defun xxx-footer (name variables)
  (...))
```

Function called at the end of parsing. It must return a string which is inserted in the output buffer.

Arguments:

- `name` (string): collection name (`unknown` if not found)
- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

#### end

```elisp
(defun xxx-end (variables)
  (...))
```

Function called at the end. It can be used to enable a major or minor mode.

Arguments:

- `variables` (alist): variables from collection and environment (in reverse order: latest variable read is the first one in alist).

## Known limitations

For now the package offers a basic support of Postman collections and environments, the following features are partially implemented:

- authentication: only `basic` and `apikey` are supported
- body: only `raw` is supported.

Pull requests are welcome to add missing features.

## Copyright

Copyright © 2020-2024 [Sébastien Helleu](https://github.com/flashcode)

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

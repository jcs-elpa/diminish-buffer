[![Build Status](https://travis-ci.com/jcs090218/diminish-buffer.svg?branch=master)](https://travis-ci.com/jcs090218/diminish-buffer)
[![MELPA](https://melpa.org/packages/diminish-buffer-badge.svg)](https://melpa.org/#/diminish-buffer)
[![MELPA Stable](https://stable.melpa.org/packages/diminish-buffer-badge.svg)](https://stable.melpa.org/#/diminish-buffer)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# diminish-buffer
> Diminish (hide) buffers from buffer-menu.

## Usage

Add the buffer name to list `diminish-buffer-list` that you want to diminish
from buffer menu.

```el
(setq diminish-buffer-list '("[*]helm" "[*]Backtrace[*]"))
```

Or you can also diminish buffer by buffer mode by tweaking the variable
`diminish-buffer-mode-list`. See the following code snippet,

```el
(setq diminish-buffer-mode-list '("Dired by name"))
```

Then enable it by calling `diminish-buffer-mode` like this.

```
M-x diminish-buffer-mode
```

The full configuration should be like this.

```el
(require 'diminish-buffer)
(diminish-buffer-mode 1)
(setq diminish-buffer-list '("*helm"))
```

Or, if you are using `use-package` to manage your plugin.

```el
(use-package diminish-buffer
  :config
  (setq diminish-buffer-list '("*helm"))
  (diminish-buffer-mode 1))
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

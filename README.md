# 🦞 Lobsters.el

A simple and elegant [Lobsters](https://lobste.rs/) client for Emacs that allows you to browse the latest tech stories directly from your favorite editor.

![Lobsters.el Screenshot](screenshot.jpg)

## Features

- 🔥 **Browse hottest stories** - View the most popular stories on Lobsters
- 🆕 **Browse newest stories** - See the latest submissions
- 📖 **Clean interface** - Minimalist design focused on readability
- ⌨️ **Keyboard navigation** - Navigate stories with simple key bindings
- 🔗 **Integrated browsing** - Open links directly in eww or external browser
- 💬 **Quick access to comments** - Jump straight to story discussions
- 🏷️ **Tag display** - See story categories at a glance
- ⏰ **Relative timestamps** - Human-readable time formatting
- ↻ **Easy refresh** - Update feeds with a single keystroke

## Installation

1. Clone this repository or download the files:
```bash
git clone https://github.com/youruser/lobsters-el.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/lobsters-el")
(require 'lobsters)
```

### Requirements

- Emacs 25.1 or later
- `request` package
- `visual-fill-column` package (version 2.4+)

Install the required packages via `package-install`:
```
M-x package-install RET request RET
M-x package-install RET visual-fill-column RET
```

## Usage

### Basic Commands

- `M-x lobsters-hottest` - View the hottest stories from Lobsters
- `M-x lobsters-newest` - View the newest stories from Lobsters

### Keyboard Shortcuts

Once in a Lobsters buffer, use these keys:

| Key | Action |
|-----|--------|
| `n` | Go to next story |
| `p` | Go to previous story |
| `r` | Refresh current feed |
| `g` | Refresh current feed (alternative) |
| `q` | Quit and close buffer |

## Customization

### Variables

You can customize these variables:

```elisp
;; Automatic refresh interval in seconds (nil to disable)
(setq lobsters-auto-refresh-interval nil)
```

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## Changelog

### Version 1.0
- Initial release
- Hottest and newest story feeds
- Keyboard navigation
- eww integration
- Clean, minimal interface
- Tag and metadata display

*Happy browsing! 🦞*

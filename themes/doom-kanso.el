;;; doom-kanso-theme.el --- A dark theme based on the Kanso Neovim theme -*- lexical-binding: t; no-byte-compile: t; -*-
;; TODO:
;; Author: Your Name <your.email@example.com>
;; Created: January 2025
;; Source: https://github.com/yourusername/kanso
;;
;;; Commentary:
;;
;; A dark theme inspired by the Kanso Neovim colorscheme, featuring
;; muted colors with excellent contrast for extended coding sessions.
;;
;;; Code:

(require 'doom-themes)

;;
;;; Variables

(defgroup doom-kanso-theme nil
  "Options for the `doom-kanso' theme."
  :group 'doom-themes)

(defcustom doom-kanso-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanso-theme
  :type 'boolean)

(defcustom doom-kanso-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanso-theme
  :type 'boolean)

(defcustom doom-kanso-comment-bg doom-kanso-brighter-comments
  "If non-nil, comments will have a subtle darker background."
  :group 'doom-kanso-theme
  :type 'boolean)

(defcustom doom-kanso-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanso-theme
  :type '(choice integer boolean))

;;; Theme definition

(def-doom-theme doom-kanso
  "A dark theme based on the Kanso Neovim colorscheme."

  ;; name        default   256         16
  ((bg         '("#090E13" "black"     "black"        )) ;; zen0
   (bg-alt     '("#090E13" "#1c1e25"   "brightblack"  )) ;; zen1
   (base0      '("#0d0c0c" "#0d0c0c"   "black"        )) ;; darker variant
   (base1      '("#1C1E25" "#1e1e1e"   "brightblack"  )) ;; zen1
   (base2      '("#22262D" "#262626"   "brightblack"  )) ;; zen2/mist0
   (base3      '("#393B44" "#3a3a3a"   "brightblack"  )) ;; zen3/mist2
   (base4      '("#4b4e57" "#4e4e4e"   "brightblack"  )) ;; inkBlack4
   (base5      '("#5C6066" "#585858"   "brightblack"  )) ;; inkGray3/mistGray3
   (base6      '("#717C7C" "#6c6c6c"   "brightblack"  )) ;; katanaGray
   (base7      '("#A4A7A4" "#a8a8a8"   "brightblack"  )) ;; inkGray
   (base8      '("#C5C9C7" "#c6c6c6"   "white"        )) ;; oldWhite/inkWhite
   (fg         '("#C5C9C7" "#c6c6c6"   "white"        )) ;; oldWhite
   (fg-alt     '("#f2f1ef" "#e4e4e4"   "brightwhite"  )) ;; fujiWhite

   (grey       base4)
   (red        '("#C34043" "#d75f5f"   "red"          )) ;; samuraiRed/autumnRed
   (orange     '("#b6927b" "#d7875f"   "brightred"    )) ;; inkOrange
   (green      '("#98BB6C" "#87af5f"   "green"        )) ;; springGreen
   (teal       '("#7AA89F" "#5fafaf"   "brightgreen"  )) ;; zenAqua2
   (yellow     '("#E6C384" "#d7af5f"   "yellow"       )) ;; carpYellow
   (blue       '("#7FB4CA" "#5fafd7"   "brightblue"   )) ;; springBlue
   (dark-blue  '("#658594" "#5f87af"   "blue"         )) ;; inkBlue
   (magenta    '("#938AA9" "#af87af"   "brightmagenta")) ;; springViolet1
   (violet     '("#a292a3" "#af87af"   "magenta"      )) ;; inkPink
   (cyan       '("#8ea4a2" "#87afaf"   "brightcyan"   )) ;; inkAqua
   (dark-cyan  '("#6A9589" "#5f8787"   "cyan"         )) ;; zenAqua1

   ;; Kanso specific colors
   (kanso-red     '("#C34043" "#ff5f87" "brightred"    )) ;; zenRed
   (kanso-green   '("#87a987" "#87af87" "green"        )) ;; inkGreen
   (kanso-yellow  '("#DCA561" "#d7af5f" "brightyellow" )) ;; autumnYellow/roninYellow
   (kanso-blue2   '("#8ba4b0" "#87afd7" "blue"         )) ;; inkBlue2
   (kanso-violet  '("#8992a7" "#8787af" "brightmagenta")) ;; inkViolet

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      base3)
   (builtin        cyan)
   (comments       (if doom-kanso-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if doom-kanso-brighter-comments base6 base5) 0.25))
   (constants      violet)
   (functions      blue)
   (keywords       magenta)
   (methods        blue)
   (operators      blue)
   (type           teal)
   (strings        green)
   (variables      fg)
   (numbers        orange)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-kanso-brighter-modeline)
   (-modeline-pad
    (when doom-kanso-padded-modeline
      (if (integerp doom-kanso-padded-modeline) doom-kanso-padded-modeline 4)))

   (modeline-fg     (if -modeline-bright base8 base7))
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken base2 0.05)
      base1))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-darken base2 0.1)
      base0))
   (modeline-bg-inactive     base1)
   (modeline-bg-inactive-alt base0))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base5)
 ((line-number-current-line &override) :foreground base7)
 ((font-lock-comment-face &override)
  :background (if doom-kanso-comment-bg (doom-lighten bg 0.05) 'unspecified))
 (mode-line
  :background modeline-bg :foreground modeline-fg
  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
 (mode-line-inactive
  :background modeline-bg-inactive :foreground modeline-fg-alt
  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
 (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

 (doom-dashboard-banner :foreground kanso-red)      ;; Already there!
 (doom-dashboard-menu-title :foreground fg)         ;; Menu items
 (doom-dashboard-menu-desc :foreground base5)       ;; Shortcuts
 (doom-dashboard-footer :foreground base5)          ;; Footer text
 (doom-dashboard-loaded :foreground base6)          ;; Loaded modules text
 (doom-dashboard-footer-icon :foreground base5)     ;; Footer icons

   ;;;; company
   (company-tooltip :background base2)
   (company-tooltip-selection :background base3)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :foreground base8)
   (doom-modeline-buffer-path :foreground base6)
   (doom-modeline-buffer-project-root :foreground base6)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background base3 :distant-foreground base0)
   (ivy-minibuffer-match-face-1 :foreground blue :background base2)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base2)
   (ivy-minibuffer-match-face-3 :foreground green :background base2)
   (ivy-minibuffer-match-face-4 :foreground yellow :background base2)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground kanso-green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base2 0.08))
   (markdown-blockquote-face :foreground base6 :slant 'italic)
   (markdown-list-face :foreground red)
   (markdown-pre-face :foreground green)
   (markdown-link-face :foreground blue)
   (markdown-url-face :foreground yellow)
   ;;;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (org-block-end-line :background base2 :foreground comments)
   (org-code :foreground yellow)
   (org-quote :background base2 :slant 'italic)
   (org-table :foreground base8)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; web-mode
   (web-mode-html-attr-name-face :foreground violet)
   (web-mode-html-attr-value-face :foreground green)
   (web-mode-html-tag-face :foreground red)
   (web-mode-css-selector-face :foreground blue)
   (web-mode-css-property-name-face :foreground green))

  ;;;; Base theme variable overrides
  ())

;;; doom-kanso-theme.el ends here

;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Required
(package! corfu-terminal)
(package! ef-themes)
(package! git-gutter-fringe)
(package! highlight-indent-guides)
(package! rainbow-delimiters)
(package! all-the-icons-dired)
;; (package! copilot :recipe (:host github :repo "zerolfx/copilot.el"))
(package! undo-tree)
(package! deadgrep)

;; Enhanced search and navigation
(package! avy)                    ; Jump to visible text quickly
(package! deadgrep)              ; Fast ripgrep interface
(package! dumb-jump)             ; Jump to definitions without tags

;; Productivity boosters
(package! helpful)               ; Better help buffers
(package! which-key)             ; Command discovery
(package! undo-tree)             ; Visualize undo history

;; Code intelligence
(package! eglot)                 ; Lightweight LSP client (alternative to lsp-mode)
;; (package! copilot
;;   :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;; UI enhancements
(package! all-the-icons-dired)   ; Icons in dired
(package! rainbow-delimiters)    ; Colorful parentheses
(package! highlight-indent-guides) ; Visual indent guides

;; Git enhancements
(package! git-gutter-fringe)     ; Git diff in fringe
(package! git-timemachine)       ; Browse file history

;; Language-specific
(package! rustic)                ; Better Rust support
(package! go-mode)               ; Enhanced Go support
(package! typescript-mode)       ; TypeScript support

;; Theme
(package! ef-themes)             ; Excellent modern themes

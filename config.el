;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; NOTE:
;; Do not need to run 'doom
;; sync' after modifying this file!
(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
;; --- Identity ---
(setq user-full-name "Pablo Aguirre"
      user-mail-address "pabloaguirrenck@protonmail.ch")

;; --- Fonts ---
(setq doom-font (font-spec :family "BerkeleyMonoRh Nerd Font" :size 24)
     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; NOTE:
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; NOTE:
;; Nerdfonts requires us to run: M-x nerd-icons-install-fonts

;; --- Shell Config ---
;; NOTE: Thhis config is suggested when using non-POSIX shell
(setq shell-file-name (executable-find "bash"))

(setq-default vterm-shell "/run/current-system/sw/bin/fish")
(setq-default explicit-shell-file-name "/run/current-system/sw/bin/fish")

;; --- UI ---
;; Theme
(setq doom-theme 'doom-kanso)

;; Theme options
(setq doom-kanso-brighter-comments nil
      doom-kanso-brighter-modeline nil
      doom-kanso-padded-modeline t)

(add-hook! '+doom-dashboard-mode-hook
  (setq-local line-spacing 0.2))

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Remove exit confirmation message
(setq confirm-kill-emacs 'nill)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Niceties
;; ---------------------------------------------------------------------
;; Performance optimizations
;; (setq gc-cons-threshold 100000000)
;; (setq read-process-output-max (* 1024 1024)) ; 1mb
;;
;; ;; Better defaults
;; (setq-default
;;  delete-by-moving-to-trash t     ; Delete files to trash
;;  window-combination-resize t     ; Resize windows proportionally
;;  x-stretch-cursor t)             ; Stretch cursor to glyph width
;;
;; ;; Enhance search with Consult (Telescope-like)
;; (after! consult
;;   ;; Preview files while searching
;;   (setq consult-preview-key 'any)
;;   
;;   ;; Custom search commands
;;   (map! :leader
;;         :desc "Search project with preview" "s P" #'consult-ripgrep
;;         :desc "Search file with preview" "s f" #'consult-line
;;         :desc "Find file recursively" "f F" #'consult-find))
;;
;; ;; Avy for quick navigation
;; (after! avy
;;   (setq avy-all-windows t
;;         avy-all-windows-alt nil
;;         avy-timeout-seconds 0.3))
;;
;; ;; Better completion
;; (after! company
;;   (setq company-idle-delay 0.2
;;         company-minimum-prefix-length 2
;;         company-show-quick-access t))
;;
;; ;; Treemacs auto-open for projects
;; (after! treemacs
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t)
;;   (treemacs-git-mode 'deferred))
;;
;; ;; Git-gutter configuration
;; (after! git-gutter
;;   (global-git-gutter-mode +1))
;;
;; ;; Which-key faster
;; (after! which-key
;;   (setq which-key-idle-delay 0.5))
;;
;; ;; Better window management
;; (map! :leader
;;       :desc "Toggle window zoom" "w m" #'doom/window-maximize-toggle
;;       :desc "Swap windows" "w x" #'ace-swap-window)
;;
;; ;; Magit improvements
;; (after! magit
;;   (setq magit-diff-refine-hunk 'all)
;;   (magit-todos-mode 1))

;; ---------------------------------------------------------------------

































;; ASCII Logo
;; ---------------------------------------------------------------------
;; Define your custom ASCII art
;; TODO: Fine-tune this obviously
;; (defun my-doom-dashboard-header ()
;;   (let* ((banner '("                                                    "
;;                    "    ██████╗ ██╗  ██╗ ██████╗ ██████╗ ██╗██╗   ██╗███╗   ███╗"
;;                    "    ██╔══██╗██║  ██║██╔═══██╗██╔══██╗██║██║   ██║████╗ ████║"
;;                    "    ██████╔╝███████║██║   ██║██║  ██║██║██║   ██║██╔████╔██║"
;;                    "    ██╔══██╗██╔══██║██║   ██║██║  ██║██║██║   ██║██║╚██╔╝██║"
;;                    "    ██║  ██║██║  ██║╚██████╔╝██████╔╝██║╚██████╔╝██║ ╚═╝ ██║"
;;                    "    ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═════╝ ╚═╝ ╚═════╝ ╚═╝     ╚═╝"
;;                    "                                                    "
;;                    "                  ▓▓▓▓▓▓▓▓▓  DOOM  ▓▓▓▓▓▓▓▓▓                "
;;                    "                                                    "))
;;          (longest-line (apply #'max (mapcar #'length banner))))
;;     (put-text-property
;;      (point)
;;      (dolist (line banner (point))
;;        (insert (+doom-dashboard--center
;;                 +doom-dashboard--width
;;                 (concat line (make-string (max 0 (- longest-line (length line))) 32)))
;;                "\n"))
;;      'face 'doom-dashboard-banner)))
;;
;; ;; Set it as your dashboard header
;; (setq +doom-dashboard-ascii-banner-fn #'my-doom-dashboard-header)

;; --- Clipboard ---
(setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq select-enable-primary t)

;; Use wl-copy/wl-paste for Wayland
(when (getenv "WAYLAND_DISPLAY")
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d '\r'")))
  
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

;; ---------------------------------------------------------------------

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

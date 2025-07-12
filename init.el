;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;; bidi                  ; (tfel ot) thgir etirw uoy gnipleh
       ;; layout                ; Auie,ctsrnm is the superior home row

       :completion
       (company +childframe)    ; The ultimate code completion backend
       consult
       ;; (corfu +orderless)       ; Complete with cap(f), cape and a flying feather!
       ;; ;;helm                   ; The *other* search engine for love and life
       ;; ;;ido                    ; The other *other* search engine...
       ;; ;; ivy                      ; A search engine for love and life
       ;; (vertico +icons)         ; The search engine of the future
       ;;
       :ui
       ;; deft                     ; Notational velocity for Emacs
       doom                     ; What makes DOOM look the way it does
       doom-dashboard           ; A nifty splash screen for Emacs
       ;;doom-quit              ; DOOM quit-message prompts when you quit Emacs
       ;; TODO: Add whenever ready
       ;; (emoji +unicode)         ; Emoji support
       hl-todo                  ; Highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides            ; Highlighted indent columns
       (ligatures +extra)       ; Ligatures and symbols to make your code pretty again
       minimap                  ; Show a map of the code on the side
       modeline                 ; Snazzy, Atom-inspired modeline, plus API
       nav-flash                ; Blink cursor line after big motions
       ;; neotree               ; A project drawer, like NERDTree for vim
       ophints                  ; Highlight the region an operation acts on
       (popup +defaults)        ; Tame sudden yet inevitable temporary windows
       smooth-scroll            ; So smooth you won't believe it's not butter
       tabs                     ; A tab bar for Emacs
       (treemacs +lsp)          ; A project drawer, like neotree but cooler
       ;; TODO: Add whenever ready
       ;; unicode                  ; Extended unicode support for various languages
       (vc-gutter +pretty)      ; Vcs diff in the fringe
       vi-tilde-fringe          ; Fringe tildes to mark beyond EOB
       window-select            ; Visually switch windows
       workspaces               ; Tab emulation, persistence & separate workspaces
       zen                      ; Distraction-free coding or writing

       :editor
       (evil +everywhere)       ; Come to the dark side, we have cookies
       file-templates           ; Auto-snippets for empty files
       fold                     ; (Nigh) universal code folding
       (format +onsave)         ; Automated prettiness
       ;;god                    ; Run Emacs commands without modifier keys
       ;;lispy                  ; Vim for lisp, for people who don't like vim
       multiple-cursors         ; Editing in many places at once
       ;;objed                  ; Text object editing for the innocent
       ;;parinfer               ; Turn lisp into python, sort of
       rotate-text              ; Cycle region at point between text candidates
       snippets                 ; My elves. They type so I don't have to
       word-wrap                ; Soft wrapping with language-aware indent

       :emacs
       (dired +icons)           ; Making dired pretty [functional]
       electric                 ; Smarter, keyword-based electric-indent
       ;;eww                    ; The internet is gross
       (ibuffer +icons)         ; Interactive buffer management
       ;; undo                     ; Persistent, smarter undo for your inevitable mistakes
       vc                       ; Version-control and Emacs, sitting in a tree

       :term
       ;; eshell                ; The elisp shell that works everywhere
       ;; shell                 ; Simple shell REPL for Emacs
       ;; term                  ; Basic terminal emulator for Emacs
       ;; vterm                    ; The best terminal emulation in Emacs

       :checkers
       syntax                   ; tasing you for every semicolon you forget
       (spell +flyspell)        ; tasing you for misspelling mispelling
       grammar                  ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio                   ; Writes a PhD for you (citation needed)
       collab                   ; Buffers with friends
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       ;; editorconfig             ; Let someone else argue about tabs vs spaces
       ein                      ; Tame Jupyter notebooks with emacs
       (eval +overlay)          ; Run code, run (also, repls)
       (lookup +docsets)        ; Navigate your code and its documentation
       ;;llm                    ; When I said you needed friends, I didn't mean...
       lsp              ; M-x vscode
       (magit +forge)           ; A git porcelain for Emacs
       make                     ; Run make tasks from Emacs
       (pass +auth)             ; Password manager for nerds
       ;; pdf                      ; PDF enhancements
       ;; rgb                      ; Color strings
       ;; taskrunner               ; Run tasks in projects
       ;; terraform                ; Infrastructure as code
       ;; tmux                     ; An API for interacting with tmux
       ;; tree-sitter              ; Syntax and parsing, sitting in a tree...
       ;; upload                   ; Map local to remote projects via ssh/ftp
       ;;
       ;; :os
       ;; tty                      ; Improve the terminal Emacs experience
       ;;
       ;; :lang
       ;; agda              ; Types of types of types of types...
       ;; beancount         ; Mind the GAAP
       ;; (cc +lsp)         ; C > C++ == 1
       ;; clojure           ; Java with a lisp
       ;; common-lisp       ; If you've seen one lisp, you've seen them all
       ;; coq               ; Proofs-as-programs
       ;; crystal           ; Ruby at the speed of c
       ;; csharp            ; Unity, .NET, and mono shenanigans
       ;; data              ; Config/data formats
       ;; (dart +flutter)   ; Paint ui and not much else
       ;; dhalL
       ;; elixir            ; Erlang done right
       ;; elm               ; Care for a cup of TEA?
       ;; emacs-lisp        ; Drown in parentheses
       ;; erlang            ; An elegant language for a more civilized age
       ;; ess               ; Emacs speaks statistics
       ;; factor
       ;; faust             ; Dsp, but you get to keep your soul
       ;; fortran           ; In FORTRAN, GOD is REAL (unless declared INTEGER)
       ;; fsharp            ; ML stands for Microsoft's Language
       ;; fstar             ; (dependent) types and (monadic) effects and Z3
       ;; gdscript          ; The language you waited for
       ;; (go +lsp)         ; The hipster dialect
       ;; (graphql +lsp)    ; Give queries a REST
       ;; (haskell +lsp)    ; A language that's lazier than I am
       ;; hy                ; Readability of scheme w/ speed of python
       ;; idris             ; A language you can depend on
       ;; (json +lsp)       ; At least it ain't XML
       ;; janet             ; Fun fact: Janet is me!
       ;; (java +lsp)       ; The poster child for carpal tunnel syndrome
       ;; (javascript +lsp) ; All(hope(abandon(ye(who(enter(here))))))
       ;; julia             ; A better, faster MATLAB
       ;; kotlin            ; A better, slicker Java(Script)
       ;; latex             ; Writing papers in Emacs has never been so fun
       ;; lean              ; For folks with too much to prove
       ;; ledger            ; Be audit you can be
       ;; lua               ; One-based indices? one-based indices
       ;; (markdown +grip)           ; Writing docs for people to ignore
       ;; nim               ; Python + lisp at the speed of c
       ;; (nix +lsp +tree-sitter)               ; I hereby declare "nix geht mehr!"
       ;; ocaml             ; An objective camel
       ;; (org +dragndrop +pandoc +pretty +roam2)               ; Organize your plain life in plain text
       ;; php               ; Perl's insecure younger brother
       ;; plantuml          ; Diagrams for confusing people more
       ;; graphviz          ; Diagrams for confusing yourself even more
       ;; purescript        ; Javascript, but functional
       ;; (python +lsp +pyright +poetry)            ; Beautiful is better than ugly
       ;; qt                ; The 'cutest' gui framework ever
       ;; racket            ; A DSL for DSLs
       ;; raku              ; The artist formerly known as perl6
       ;; rest              ; Emacs as a REST client
       ;; rst               ; ReST in peace
       ;; (ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; scala             ; Java, but good
       ;; (scheme +guile)   ; A fully conniving family of lisps
       ;; (sh +lsp)                ; She sells {ba,z,fi}sh shells on the C xor
       ;; sml
       ;; solidity          ; Do you need a blockchain? No.
       ;; swift             ; Who asked for emoji variables?
       ;; terra             ; Earth and Moon in alignment for performance.
       ;; (web +lsp)               ; The tubes
       ;; (yaml +lsp)              ; JSON, but readable
       ;; zig               ; C, but simpler
       ;;
       ;; :email
       ;; ;;(mu4e +org +gmail)
       ;; ;;notmuch
       ;; ;;(wanderlust +gmail)
       ;;
       ;; :app
       ;; calendar
       ;; ;;emms
       ;; ;;everywhere        ; *leave* Emacs!? You must be joking
       ;; irc               ; how neckbeards socialize
       ;; (rss +org)        ; emacs as an RSS reader
       ;;
       ;; :config
       ;; literate
       ;; (default +bindings +smartparens)
)

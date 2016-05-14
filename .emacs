;; User dettail
(setq user-full-name "Ayonga Hereid")
(setq user-mail-address "ayonga27@gmail.com")

;; common-lisp environment
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(require 'cl)

;; pacakge management
(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar ayonga/packages '(auto-complete
			  auctex
			  smex
			  org
			  smartparens
                          auto-complete-auctex
                          yaml-mode
			  flx-ido
			  ido-ubiquitous
			  undo-tree
			  diminish
			  gitconfig-mode
			  gitignore-mode
			  volatile-highlights
			  ac-math
			  anzu
			  gandalf-theme
			  writegood-mode
			  occidental-theme
			  soft-stone-theme
			  zenburn-theme
			  sunny-day-theme
			  magit
			  markdown-mode)
 
  "Default packages")


;;; Install default packages

(defun ayonga/packages-installed-p ()
  (loop for pkg in ayonga/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ayonga/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ayonga/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;; start-up options
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq-default cursor-type 'bar)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))
(global-set-key [s-left] 'windmove-left) 
(global-set-key [s-right] 'windmove-right) 
(global-set-key [s-up] 'windmove-up) 
(global-set-key [s-down] 'windmove-down)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; indention
(setq tab-width 4
      indent-tabs-mode nil)
;; get rid of backup files
(setq make-backup-files nil)
(setq tab-always-indent 'complete)


(defalias 'yes-or-no-p 'y-or-n-p)
;; key binding
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; color theme
(load-theme 'zenburn t)

;; smex
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ido-mode, navigating file systems
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; editor options
;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)
;; highlight the current line
(global-hl-line-mode nil)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)


;; anzu-mode enhances isearch by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 
(global-set-key (kbd "C-S-z") 'redo) ; 

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; deal with temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; enable auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.5)

;; flyspell
;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-list-command "list")

;; yaml mode hook
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))



;;; smartparens mode 
(require 'smartparens-latex)
(smartparens-global-mode 0)

;; setup auctex

(require 'tex-site)
;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)
;; Only parse LaTeX class and package information.
(setq-default TeX-auto-regexp-list 'LaTeX-auto-full-regexp-list)
;; The class and package information is usually near the beginning.
(setq-default TeX-auto-parse-length 999999)
;; automatic customizatoin
(setq TeX-macro-global '("/usr/local/texlive/2015/texmf-dist/tex/"))
(setq TeX-macro-private '("/home/ayonga/texmf/tex/"))
(setq TeX-auto-local '("auto/")) ; set path for parsed stuff
;; use pdflatex
(add-hook 'LaTeX-mode-hook 'server-start)
(setq TeX-PDF-mode t)

;; sensible defaults for OS X, other OSes should be covered out-of-the-box

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; reftex

(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)
(setq LaTeX-eqnarray-label "eq:"
      LaTeX-amsmath-label "eq:"
      LaTeX-equation-label "eq:"
      LaTex-align-label "eq:"
      LaTeX-figure-label "fig:"
      LaTeX-table-label "tab:"
      LaTex-section-label "sec:"
      LaTex-subsection-label "sec:"
      LaTeX-myChapter-label "chap:"
      TeX-newline-function 'reindent-then-newline-and-indent
      TeX-style-path
      '("styles/")
      LaTeX-section-hook
      '(LaTeX-section-heading
	LaTeX-section-title
	LaTeX-section-toc
	LaTeX-section-section
	LaTeX-section-label))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
(setq reftex-default-bibliography '("/home/ayonga/Dropbox/bibliography/ayonga.bib"
				    "/home/ayonga//Dropbox/bibliography/hzd.bib"
				    "/home/ayonga//Dropbox/bibliography/optimization.bib"
				    "/home/ayonga//Dropbox/bibliography/control.bib"
				    "/home/ayonga//Dropbox/bibliography/robotics.bib"))
;; set XeTeX mode in TeX/LaTeX
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
             (setq TeX-command-default "LaTeX")
             (setq TeX-save-query nil)
             (setq TeX-show-compilation t)))

;; syctex
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(org-export-backends (quote (ascii beamer html icalendar latex md)))
 '(safe-local-variable-values (quote ((Tex-engine . XeLaTex))))
 '(send-mail-function nil))


(setq TeX-view-program-selection
      '((output-pdf "Okular")))
(setq TeX-view-program-list
      '(("PDF Viewer" "okular --unique %o#src:%n%b")))


;; auto complete on AUCTex
(require 'auto-complete-auctex)
  (add-to-list 'load-path "/home/ayonga/.emacs.d/predictive")
  (add-to-list 'load-path "/home/ayonga/.emacs.d/predictive/latex")
  (add-to-list 'load-path "/home/ayonga/.emacs.d/predictive/texinfo")
  (add-to-list 'load-path "/home/ayonga/.emacs.d/predictive/html")
  (add-to-list 'load-path "/home/ayonga/.emacs.d/predictive/misc")
  (require 'predictive)
(setq completion-use-hotkeys pop-up-frames)

;(add-hook 'LaTeX-mode-hook 'turn-on-predictive-mode)
;(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ;;turn on cdlatex

(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
               ac-sources)))

(add-hook 'Latex-mode-hook 'ac-latex-mode-setup)


(ac-flyspell-workaround)

;; Setting up writegood-mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;;Automatically insert non-breaking space before citation
(setq reftex-format-cite-function
          '(lambda (key fmt)
	     (let ((cite (replace-regexp-in-string "%l" key fmt)))
	       (if (or (= ?~ (string-to-char fmt))
		       (member (preceding-char) '(?\ ?\t ?\n ?~)))
	           cite
	         (concat "~" cite)))))
;; Change this to the place where you store all the electronic versions.
(defvar bibtex-papers-directory "~/Dropbox/bibliography/ayonga_files/")

;; Translates a BibTeX key into the base filename of the corresponding
;; file. Change to suit your conventions.
;; Mine is:
;; - author1-author2-author3.conferenceYYYY for the key
;; - author1-author2-author3_conferenceYYYY.{ps,pdf} for the file
(defun bibtex-key->base-filename (key)
  (concat bibtex-papers-directory
          (replace-regexp-in-string "\\." "_" key)))

;; Finds the BibTeX key the point is on.
;; You might want to change the regexp if you use other strange characters in the keys.
(defun bibtex-key-at-point ()
  (let ((chars-in-key "A-Z-a-z0-9_:-\\."))
    (save-excursion
      (buffer-substring-no-properties
       (progn (skip-chars-backward chars-in-key) (point))
       (progn (skip-chars-forward chars-in-key) (point))))))

;; Opens the appropriate viewer on the electronic version of the paper referenced at point.
;; Again, customize to suit your preferences.
(defun browse-paper-at-point ()
  (interactive)
  (let ((base (bibtex-key->base-filename (bibtex-key-at-point))))
    (cond
     ((file-exists-p (concat base ".pdf"))
      (shell-command (concat "evince " base ".pdf &")))
     ((file-exists-p (concat base ".ps"))
      (shell-command (concat "gv " base ".ps &")))
     (t (message (concat "No electronic version available: " base ".{pdf,ps}"))))))

(global-set-key [S-f6] 'browse-paper-at-point)

;; Git (Magit key bindings)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Markdown main mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t))))

(global-visual-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

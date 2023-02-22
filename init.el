;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; leafの推奨設定
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 雑多なパッケージ
;;; use-package
(leaf use-package :ensure t :require t)

;;; smartparens
(leaf smartparens
  :ensure t)
(smartparens-global-mode t)

;; neotree
(leaf neotree
  :ensure t
  :bind (("" . neotree-toggle))
  :custom ((neo-theme quote ascii)
           (neo-persist-show . t)
           (neo-smart-open . t)))

;; comapany
(leaf company
  :ensure t
  :bind (("C-M-i" . company-complete)
         (company-active-map
          ("C-n" . company-select-next))
         (company-active-map
          ("C-p" . company-select-previous))
         (company-search-map
          ("C-n" . company-select-next))
         (company-search-map
          ("C-p" . company-select-previous))
         (company-active-map
          ("C-s" . company-filter-candidates))
         (company-active-map
          ("C-i" . company-complete-selection))
         (company-active-map
          ([tab]
           . company-complete-selection))
         (company-active-map
          ("C-f" . company-complete-selection))
         (emacs-lisp-mode-map
          ("C-M-i" . company-complete)))
  :custom ((company-transformers quote
                                  (company-sort-by-backend-importance))
            (company-idle-delay . 0)
            (company-minimum-prefix-length . 3)
            (company-selection-wrap-around . t)
            (completion-ignore-case . t)
            (company-dabbrev-downcase))
  :config
  (global-company-mode))

;; yasnippet
(leaf yasnippet
  :ensure t)

;; magit
(leaf magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;; emogify
(leaf emojify
  :ensure t
  :commands global-emojify-mode
  :hook ((after-init-hook . global-emojify-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(defvar org-todo-keywords "")
(setq org-todo-keywords
      '((sequence "TODO" "SOMEDAY" "WAITING" "|" "DONE")))
(leaf org-journal
  :ensure t
  :config
  (let ((custom--inhibit-theme-enable nil))
    (unless (memq 'use-package custom-known-themes)
      (deftheme use-package)
      (enable-theme 'use-package)
      (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
    (custom-theme-set-variables 'use-package
                                '(org-journal-dir "~/Dropbox/emacs/org/journal" nil nil "Customized with use-package org-journal")
                                '(org-journal-date-format "%A, %d %B %Y" nil nil "Customized with use-package org-journal"))))

(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (goto-char (point-min)))
(setq org-log-done 'time)
(setq org-use-speed-commands t)
(setq org-src-fontify-natively t)
(setq org-agenda-files '("~/Dropbox/emacs/org/task.org"))

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)

; Org-captureのテンプレート（メニュー）の設定
(defvar org-capture-templates "")
(setq org-capture-templates
      '(("e" "Experiment" entry (file+headline "~/Dropbox/emacs/org/experiment.org" "Experiment")
	 "* %? %U %i\n
#+BEGIN_SRC emacs-lisp

#+END_SRC")
	("i" "Idea" entry (file+headline "~/Dropbox/emacs/org/idea.org" "Idea")
	 "* %? %U %i")
	("r" "Remember" entry (file+headline "~/Dropbox/emacs/org/remember.org" "Remember")
	 "* %? %U %i")
	("m" "Memo" entry (file+headline "~/Dropbox/emacs/org/memo.org" "Memo")
	 "* %? %U %i")
	("t" "Task" entry (file+headline "~/Dropbox/emacs/org/inbox.org" "Task")
	 "** TODO %? \n   SCHEDULED: %^t \n")
    ("j" "Journal entry" entry (function org-journal-find-location)
     "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(defvar org-refile-targets "")
(setq org-refile-targets
      (quote (("~/Dropbox/emacs/org/archives.org" :level . 1)
	      ("~/Dropbox/emacs/org/remember.org" :level . 1)
	      ("~/Dropbox/emacs/org/memo.org" :level . 1)
	      ("~/Dropbox/emacs/org/task.org" :level . 1))))

;; メモをC-M-^一発で見るための設定
;; https://qiita.com/takaxp/items/0b717ad1d0488b74429d から拝借
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Dropbox/emacs/org/" file))))
(global-set-key (kbd "C-M-^ i") '(lambda () (interactive)
                                   (show-org-buffer "inbox.org")))
(global-set-key (kbd "C-M-^ t") '(lambda () (interactive)
                                   (show-org-buffer "task.org")))
(global-set-key (kbd "C-M-^ m") '(lambda () (interactive)
                                 (show-org-buffer "memo.org")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :after org
  :defer t
  :custom
  (org-roam-db-update-method 'immediate)
  (org-roam-db-location "~/.emacs.d/org-roam.db")
  (org-roam-directory "~/Dropbox/emacs/org/org-roam/")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-insert-immediate)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-enable)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ddskk
(leaf ddskk
  :bind (("C-x C-j" . skk-mode)
         (minibuffer-local-map :package ddskk
                               ("C-j" . skk-kakutei)))
  :custom ((default-input-method . "japanese-skk")
           (skk-preload . t)
           (skk-auto-insert-paren . t))
  :config
  (leaf-handler-package ddskk ddskk nil)
  (with-eval-after-load 'ddskk
    (add-hook 'skk-load-hook
              (lambda nil
                (custom-set-variables skk-rom-kana-rule-list (nconc skk-rom-kana-rule-list
                                                                    '((";" nil nil)
                                                                      (":" nil nil)
                                                                      ("?" nil nil)
                                                                      ("!" nil nil))))))))
(leaf sticky
  :ensure ddskk
  :custom ((skk-sticky-key . ";")))
(setq skk-large-jisyo "~/.SKK-JISYO.L")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico関連
(leaf vertico
  :ensure t
  :require t
  :custom (vertico-count . 10))
(leaf consult
  :ensure t
  :require t)
(leaf marginalia
  :ensure t
  :config (marginalia-mode))
(leaf embark
  :ensure t)
;; 補完スタイルにorderlessを利用する
(leaf orderless
  :ensure t
  :custom (completion-styles . '(orderless)))

;; vertico-modeとmarginalia-modeを有効化する
(defun after-init-hook ()
  (vertico-mode)
  (marginalia-mode)
  ;; savehist-modeを使ってVerticoの順番を永続化する
  (savehist-mode))
(add-hook 'after-init-hook #'after-init-hook)

;; embark-consultを読み込む
(leaf embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
(defun my-consult-line (&optional at-point)
  "Consult-line uses things-at-point if set C-u prefix."
  (interactive "P")
  (if at-point
      (consult-line (thing-at-point 'symbol))
    (consult-line)))

;; C-s（isearch-forward）をmy-consult-lineコマンドに割り当てる
(global-set-key (kbd "C-s") 'my-consult-line)

;; C-s/C-rで行を移動できるようにする
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 見映え
;;; modus-mode
(leaf modus-themes
  :ensure t
  :bind (("<f5>" . my-modus-themes-toggle))
  :custom ((modus-themes-italic-constructs . t)
           (modus-themes-bold-constructs)
           (modus-themes-region quote
								(bg-only no-extend))))

(defun my-modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
This uses `enable-theme' instead of the standard method of
`load-theme'.  The technicalities are covered in the Modus themes
manual."
  (interactive)
  (pcase (modus-themes--current-theme)
    ('modus-vivendi (modus-themes-select 'modus-operandi-tinted))
	('modus-operandi-tinted (modus-themes-select 'modus-vivendi))
    (_ (error "No Modus theme is loaded; evaluate `modus-themes-load-themes' first"))))
(modus-themes-select 'modus-vivendi)

;;; alpha
(if window-system 
    (progn
      (set-frame-parameter nil 'alpha 92)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Server
(use-package lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook ((rust-mode . lsp)
         (python-mode .lsp))
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c C-i" . lsp-ui-doc-focus-frame))
  :custom (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t)

(setq read-process-output-max (* 1024 1024))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust用設定
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C言語用設定(手探り中)
(setq-default c-basic-offset 4     ;;基本インデント量4
              tab-width 4          ;;タブ幅4
               indent-tabs-mode t)  ;;インデントをタブでするかスペースでするか

;; C++ style
(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")  ;;スタイルはストラウストラップ
  (show-paren-mode t))        ;;カッコを強調表示する
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;; C style
(defun add-c-mode-common-conf ()
  (c-set-style "stroustrup")                  ;;スタイルはストラウストラップ
  (show-paren-mode t)                         ;;カッコを強調表示する
  )
(add-hook 'c-mode-common-hook 'add-c-mode-common-conf)

;; ccls
(use-package ccls
  :ensure t)
(setq ccls-executable "/var/lib/snapd/snap/bin/ccls")
(add-hook 'c-mode-hook #'lsp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go
(setq exec-path (cons (expand-file-name "~/go/bin") exec-path))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook #'lsp)
(defun go-mode-omnibus ()
  ;; Go code formatting by goimports
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )
(add-hook 'go-mode-hook 'go-mode-omnibus)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime
(setq inferior-lisp-program "sbcl")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;; https://tam5917.hatenablog.com/entry/2022/01/23/234718
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

;; 保存時に自動整形
(add-hook 'python-mode-hook
          #'(lambda ()
              (add-hook 'before-save-hook
                        'lsp-format-buffer)))
(add-hook 'python-mode-hook #'python-isort-on-save-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mark-ring
(leaf back-button
  :ensure t
  :require t)
(back-button-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; superキーを使いたい(lspの中で定義されている)
(defun shani-cheer ()
  (interactive)
  (shell-command "~/Dropbox/random_open.sh shani_cheer"))
(bind-key "s-@" #'shani-cheer)
;;; kill-this-buffer
(bind-key "s-k" #'kill-this-buffer)

(provide 'init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hydra
(leaf hydra
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf key-chord :ensure t)
    (leaf bind-key :ensure t)
    (leaf swiper :ensure t)
    (leaf diff-hl :ensure t))

(key-chord-define-global
 "jk"
 (defhydra hydra-pinky
   ()
   "pinky"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ("V" scroll-down-command)
   ("g" keyboard-quit)
   ("j" diff-hl-next-hunk)
   ("k" diff-hl-previous-hunk)
   ("o" other-window-or-split)
   ("r" avy-goto-word-1)
   ("l" recenter-top-bottom)
   ("s" swiper-isearch-region)
   ("S" window-swap-states)
   ("q" kill-buffer)
   ("w" clipboard-kill-ring-save)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("SPC" set-mark-command)
   ("\C-m" dired-find-file)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("0" delete-window)
   ("x" delete-window)
   (";" counsel-switch-buffer)
   ("M-n" next-buffer)
   ("M-p" previous-buffer)))

;; key-chord
(setq key-chord-two-keys-delay 0.1)
(key-chord-mode 1)
(bind-key "C-'" 'hydra-pinky/body)

(defun other-window-or-split ()
  "If there is one window, open split window.
If there are two or more windows, it will go to another window."
  (interactive)
  (when (one-window-p)
	(split-window-vertically))
  (other-window 1))

(defun swiper-isearch-region ()
  "If region is selected, `swiper-isearch' with the keyword selected in region.
If the region isn't selected, `swiper-isearch'."
  (interactive)
  (if (not (use-region-p))
      (swiper-isearch)
    (swiper-isearch-thing-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 起動時間測定
;; 参考：https://zenn.dev/takeokunn/articles/56010618502ccc#%E8%A8%88%E6%B8%AC%E6%96%B9%E6%B3%95
(defconst my/before-load-init-time (current-time))

;;;###autoload
(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defvar my/tick-previous-time my/before-load-init-time)

;;;###autoload
(defun my/tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my/loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my/tick-previous-time)))
               msg)
      (setq my/tick-previous-time ctime))))

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my/emacs-init-time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 3)
 '(company-selection-wrap-around t)
 '(company-transformers '(company-sort-by-backend-importance))
 '(completion-ignore-case t t)
 '(completion-styles '(orderless))
 '(default-input-method "japanese-skk" nil nil "Customized with leaf in `ddskk' block")
 '(display-time-mode t)
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(imenu-list-position 'left t)
 '(imenu-list-size 30 t)
 '(modus-themes-bold-constructs nil)
 '(modus-themes-italic-constructs t)
 '(modus-themes-region '(bg-only no-extend))
 '(neo-persist-show t t)
 '(neo-smart-open t)
 '(neo-theme 'ascii)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
	 ("melpa" . "https://melpa.org/packages/")
	 ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(cargo bind-key diff-hl key-chord swiper python-isort python-mode tramp consult-org-roam org-roam-ui org-roam slime totp tuareg flycheck flycheck-golangci-lint flycheck-rust go-eldoc go-mode rjsx-mode emojify org-journal smartparens-config smartparens-lisp magit modus-themes macrostep leaf-tree leaf-convert hydra el-get blackout))
 '(show-paren-mode t)
 '(skk-auto-insert-paren t)
 '(skk-preload t)
 '(skk-sticky-key ";")
 '(vertico-count 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 120 :width normal)))))
;; Local Variables:
;; indent-tabs-mode: nil
;; End:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 起動時間高速化後処理
(setq file-name-handler-alist my-saved-file-name-handler-alist)

;;; init.el ends here

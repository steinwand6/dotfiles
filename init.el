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

;;; smartparens (要らない気がしてきたのでデフォで有効にはしない)
(leaf smartparens
  :ensure t)
;;;;(smartparens-global-mode t)

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
;; 基本設定
;;; encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
;;; other-windowをC-tに置き変える
(global-set-key (kbd "C-t") 'other-window)
;;; 括弧の自動補完機能らしい
(electric-pair-mode 1)
;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)
;;; splash screenを無効にする
(setq inhibit-splash-screen t)
;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)
;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)
;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")
;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
;;; 釣合う括弧をハイライトする
(show-paren-mode 1)
;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)
;;; 現在行に色をつける
(global-hl-line-mode 1)
;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)
;;; モードラインに時刻を表示する
(display-time)
;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)
;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))
;;; ログの記録行数を増やす
(setq message-log-max 10000)
;;; 履歴をたくさん保存する
(setq history-length 1000)
;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;; 行番号表示
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      (set-face-attribute 'line-number nil
                          :foreground "DarkOliveGreen"
                          :background "#131521")
      (set-face-attribute 'line-number-current-line nil
                          :foreground "gold")))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
;;; C-x oの代わりのバッファ移動
(global-set-key "\C-cl" 'windmove-right)
(global-set-key "\C-ch" 'windmove-left)
(global-set-key "\C-cj" 'windmove-down)
(global-set-key "\C-ck" 'windmove-up)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(defvar org-todo-keywords "")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "SOMEDAY" "WAITING" "|" "DONE")))
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
	("t" "Task" entry (file+headline "~/Dropbox/emacs/org/task.org" "Task")
	 "** TODO %? \n   SCHEDULED: %^t \n")
    ("j" "Journal entry" entry (function org-journal-find-location)
     "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(defvar org-refile-targets "")
(setq org-refile-targets
      (quote (("~/Dropbox/emacs/org/archives.org" :level . 1)
	      ("~/Dropbox/emacs/org/remember.org" :level . 1)
	      ("~/Dropbox/emacs/org/memo.org" :level . 1)
	      ("~/Dropbox/emacs/org/task.org" :level . 1))))


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
  :bind (("<f5>" . modus-themes-toggle))
  :custom ((modus-themes-italic-constructs . t)
           (modus-themes-bold-constructs)
           (modus-themes-region quote
                              (bg-only no-extend)))
  :config
  (modus-themes-load-themes)
  (with-eval-after-load 'modus-themes
    (modus-themes-load-operandi)))
;;; alpha
(if window-system 
    (progn
      (set-frame-parameter nil 'alpha 92)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust用設定
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
;;; rust
(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))
;;; lsp
(use-package lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook (rust-mode . lsp)
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c C-i" . lsp-ui-doc-focus-frame))
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)


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
   '(go-eldoc go-mode rjsx-mode emojify org-journal smartparens-config smartparens-lisp magit modus-themes macrostep leaf-tree leaf-convert hydra el-get blackout))
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

;;; init.el ends here

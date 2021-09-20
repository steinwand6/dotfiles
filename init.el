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
;; 必須パッケージ
;;; use-package
(leaf use-package :ensure t :require t)

;;; smartparens (要らないかも)
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
(defvar org-directory "")
(defvar org-default-notes-file "")
(setq org-directory "~/Documents/org")
(setq org-default-notes-file "notes.org")
;; Org-captureを呼び出すキーシーケンス
(global-set-key (kbd "C-c c") 'org-capture)
; Org-captureのテンプレート（メニュー）の設定
(defvar org-capture-templates "")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/org/gtd.org" "INBOX")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+headline "~/Documents/org/notes.org" "Notes")
         "* %?\nEntered on %U\n %i\n %a")))
;; メモをC-M-^一発で見るための設定
;; https://qiita.com/takaxp/items/0b717ad1d0488b74429d から拝借
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (concat "~/Documents/org/" file))))
(global-set-key (kbd "C-M-^") '(lambda () (interactive)
                                 (show-org-buffer "gtd.org")))
(defvar org-agenda-files "")
(setq org-agenda-files '("~/Documents/org"))
(defvar org-refile-targets "")
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(defvar org-todo-keywords "")
(setq org-todo-keywords
  '((sequence "TODO" "SOMEDAY" "WAITING" "|" "DONE")))


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
  :bind ("C-c d" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)

(provide 'init)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(default-input-method "japanese-skk" nil nil "Customized with leaf in `ddskk' block")
 '(display-time-mode t)
 '(imenu-list-position 'left t)
 '(imenu-list-size 30 t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(smartparens-config smartparens-lisp magit modus-themes macrostep leaf-tree leaf-convert hydra el-get blackout))
 '(show-paren-mode t))
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

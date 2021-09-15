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

;; ここにいっぱい設定を書く
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

;; use-package
(leaf use-package :ensure t :require t)

;; るびきちさんのおすすめ最低限設定
;;; other-windowをC-x tに置き変える
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
;;ここまでるびきちさん

;;; C-x oの代わりのバッファ移動
(global-set-key "\C-cl" 'windmove-right)
(global-set-key "\C-ch" 'windmove-left)
(global-set-key "\C-cj" 'windmove-down)
(global-set-key "\C-ck" 'windmove-up)

;; neotreeの設定
(leaf neotree
  :ensure t
  :bind (("" . neotree-toggle))
  :setq ((neo-theme quote ascii)
         (neo-persist-show . t)
         (neo-smart-open . t)))

;; ddskk
(leaf ddskk
  :ensure t
  :bind (("C-x C-j" . skk-mode)
         (minibuffer-local-map
          ("C-j" . skk-kakutei)))
  :setq ((default-input-method . "japanese-skk")
         (skk-preload . t)
         (skk-auto-insert-paren . t))
  :config
  (add-hook 'skk-load-hook
            (lambda nil
              (setq skk-rom-kana-rule-list (nconc skk-rom-kana-rule-list
                                                  '((";" nil nil)
                                                    (":" nil nil)
                                                    ("?" nil nil)
                                                    ("!" nil nil))))))
  (add-hook 'web-mode-hook
            (lambda nil
              (skk-mode)
              (skk-latin-mode-on))))
(leaf sticky
  :ensure ddskk
  :setq ((skk-sticky-key . ";")))

;; modus-mode
(leaf modus-themes
  :ensure t
  :bind (("<f5>" . modus-themes-toggle))
  :setq ((modus-themes-italic-constructs . t)
         (modus-themes-bold-constructs)
         (modus-themes-region quote
                              (bg-only no-extend)))
  :config
  (modus-themes-load-themes)
  (with-eval-after-load 'modus-themes
    (modus-themes-load-operandi)))

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
  :setq ((company-transformers quote
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
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(imenu-list-position 'left t)
 '(imenu-list-size 30 t)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(neotree modus-themes macrostep leaf-tree leaf-convert hydra el-get blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here

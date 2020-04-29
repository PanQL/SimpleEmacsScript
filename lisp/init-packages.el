(require 'package)
(package-initialize)
(setq package-archives '(("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
			 ("gnu-elpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      ;; --- Auto-completion ---
		      company
		      company-lsp
		      ;; --- Rust mode ---
		      rust-mode
		      ;; --- Themes ---
		      monokai-theme
		      ;; --- AUCTex ---
		      auctex
		      ;; --- 从shell设置的环境变量中读取环境变量内容 ---
		      exec-path-from-shell
		      ;; --- 优化窗口切换 ---
		      ace-window
		      ;; --- 语法检查 ---
		      flycheck
		      ;; --- rust语法检查 ---
		      flycheck-rust
		      ;; --- git ---
		      magit
		      ;; --- avy for line-jump ---
		      avy
		      lsp-ui
		      yasnippet
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; 对所有buffer开启company
(add-hook 'after-init-hook 'global-company-mode)
;; 设置company-lsp为company后端
(require 'company-lsp)
(push 'company-lsp company-backends)
(setq company-idle-delay 0.2) ; set the completion menu pop-up delay
(setq company-minimum-prefix-length 1) ; pop up a completion menu by tapping a character

;; BEGIN: rust语言相关设置
;; rust mode相关设置
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq company-tooltip-align-annotations t)
(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook 'hs-minor-mode)
;; END: rust语言相关设置

;; BEGIN:auctex相关设置
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	   (setq TeX-engine 'xetex
                 TeX-PDF-mode t)
	   (setq TeX-save-query nil)
	   (setq TeX-auto-save t)
           (setq TeX-parse-self t)
           (setq-default TeX-master nil)))
;; END:auctex相关设置

;; BEGIN:exec-path-from-shell相关设置
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; END:exec-path-from-shell相关设置

;; BEGIN:ace-window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(ace-window-display-mode t)
;; END:ace-window

(provide 'init-packages)

;; for ace-window
;; 触发窗口切换
(global-set-key (kbd "M-p") 'ace-window)

;; for avy
;; 行跳转
(global-set-key (kbd "M-g f") 'avy-goto-line)

;; for swiper
;; 替换默认的isearch
(global-set-key (kbd "C-s") 'swiper-isearch)
(provide 'init-key)

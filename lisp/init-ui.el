;; 关闭工具栏
(tool-bar-mode -1)

;; 关闭文件滑动按钮
(scroll-bar-mode -1)

;; 关闭菜单栏
(menu-bar-mode -1)

;; 设置光标样式
(setq-default cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 开启默认全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 设置主题
(load-theme 'monokai t)

;; 高亮当前行
(global-hl-line-mode 1)

;; 设置当前行背景色
;;(set-face-background 'hl-line "#BEBEBE")

(provide 'init-ui)

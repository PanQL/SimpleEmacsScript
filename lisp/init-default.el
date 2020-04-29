;; 取消备份
(setq make-backup-files nil)

;; 选中一段文字之后输入的字符会替换选中部分
(delete-selection-mode 1)

;; 自动加载外部修改过的文件
(global-auto-revert-mode 1)

;; 关闭自动保存文件
(setq auto-save-default nil)

;; 关闭警告音
(setq ring-bell-function 'ignore)

(provide 'init-default)

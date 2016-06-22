(let ((git-dir (file-name-directory
                (directory-file-name default-directory))))
  (add-to-list 'load-path (expand-file-name "lispy" git-dir))
  (add-to-list 'load-path (expand-file-name "worf" git-dir))
  (add-to-list 'load-path (expand-file-name "zoutline" git-dir))
  (add-to-list 'load-path (expand-file-name "ace-link" git-dir))
  (add-to-list 'load-path (expand-file-name "avy" git-dir))
  (add-to-list 'load-path (expand-file-name "iedit" git-dir))
  (add-to-list 'load-path (expand-file-name "swiper" git-dir))
  (add-to-list 'load-path (expand-file-name "hydra" git-dir)))


(setq files '("pamparam.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)

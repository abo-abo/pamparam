(setq files '("pamparam.el"))
(setq byte-compile--use-old-handlers nil)
(mapc #'byte-compile-file files)

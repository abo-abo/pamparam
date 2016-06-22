;;; pamparam.el --- Simple and fast flashcards. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/pamparam
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.3") (lispy "0.26.0") (worf "0.1.0"))
;; Keywords: flashcards, memory

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;* Requires
(require 'worf)
(require 'lispy)

;;* Pure
(defun pam-sm2 (card-stats q)
  "Determine the next iteration of CARD-STATS based on Q.

CARD-STATS is (EASE-FACTOR . INTERVALS), the result has the
same shape, with updated values.

EASE-FACTOR - the previous ease factor of the card. All cards are
initialized with EASE-FACTOR of 2.5.  It will decrease for
difficult cards, but not below 1.3.

INTERVALS - list of integer day intervals between repetitions.

Q - the quality of the answer:
  5 - perfect response
  4 - correct response after a hesitation
  3 - correct response recalled with serious difficulty
  2 - incorrect response; where the correct one seemed easy to recall
  1 - incorrect response; the correct one remembered
  0 - complete blackout"
  (let ((EF (car card-stats))
        (intervals (cdr card-stats)))
    (setq EF (max 1.3 (+ EF 0.1 (* (- q 5) (+ 0.08 (* (- 5 q) 0.02))))))
    (if (< q 3)
        (cons EF (cons 1 intervals))
      (cons EF
            (cons
             (cond ((null intervals)
                    1)
                   ((= (car intervals) 1)
                    6)
                   (t
                    (round (* EF (car intervals)))))
             intervals)))))

;;* Card files
(defun pam-card-insert-score (score actual-answer)
  "Insert SCORE into the current card file."
  (goto-char (point-min))
  (outline-show-all)
  (if (re-search-forward "^\\*\\* scores" nil t)
      (outline-end-of-subtree)
    (forward-line 2)
    (insert "** scores\n")
    (backward-char))
  (when actual-answer
    (kill-new actual-answer))
  (insert (format-time-string "\n| <%Y-%m-%d> ")
          (format "| %d |" score)
          (format " %s |"
                  (or actual-answer "")))
  (org-table-align))

(defun pam-card-read-stats ()
  (goto-char (point-min))
  (if (re-search-forward "^\\*\\* stats\n" nil t)
      (let ((beg (point))
            (exp1 (read (current-buffer)))
            (exp2 (read (current-buffer)))
            ease-factor intervals)
        (if (and (eq (nth 0 exp1) 'setq)
                 (eq (nth 1 exp1) 'ease-factor)
                 (numberp (nth 2 exp1)))
            (setq ease-factor (nth 2 exp1))
          (error "Bad sexp %S" exp1))
        (if (and (eq (nth 0 exp2) 'setq)
                 (eq (nth 1 exp2) 'intervals))
            (setq intervals (cadr (nth 2 exp2)))
          (error "Bad sexp %S" exp2))
        (delete-region beg (point))
        (cons ease-factor intervals))
    (if (re-search-forward "^\\*\\* scores\n" nil t)
        (progn
          (outline-end-of-subtree)
          (insert "\n** stats\n")
          (list 2.5))
      (error "** scores not found"))))

(defun pam-card-insert-stats (stats)
  (insert (format "(setq ease-factor %f)\n" (car stats)))
  (insert (format "(setq intervals '%S)" (cdr stats))))

(defun pam-delete-region (beg end)
  (let ((str (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    str))

(defun pam-save-buffer ()
  (write-file (buffer-file-name)))

(defun pam-card-score (score &optional actual-answer)
  (interactive)
  (let* ((card-file (file-name-nondirectory (buffer-file-name)))
         (state (with-current-buffer (pam-todo-file)
                  (goto-char (point-min))
                  (search-forward card-file)
                  (goto-char (+ 2 (line-beginning-position)))
                  (buffer-substring-no-properties
                   (point)
                   (progn
                     (forward-word)
                     (point)))))
         (save-silently t))
    (cond ((string= state "REVIEW")
           (with-current-buffer (pam-todo-file)
             (goto-char (point-min))
             (search-forward card-file)
             (if (or (= score 5)
                     (= score 4)
                     (= score 3))
                 (let ((org-log-done nil))
                   (org-todo 'done))
               (let ((item (pam-delete-region
                            (line-beginning-position)
                            (1+ (line-end-position)))))
                 (goto-char (point-max))
                 (insert item)))
             (pam-save-buffer))
           (pam-save-buffer))
          ((string= state "DONE")
           (if (y-or-n-p "Card already done today. Re-rate?")
               (pam--card-score score t actual-answer)
             (user-error "This card is already done today")))
          ((string= state "TODO")
           (pam--card-score score nil actual-answer))
          (t
           (user-error "unexpected state: %s" state)))
    (outline-show-all)))

(defun pam--todo-from-file (card-file)
  (if (string-match "\\`\\([^-]+\\)-" card-file)
      (format
       "* TODO [[file:cards/%s][%s]]\n"
       card-file
       (match-string 1 card-file))
    (error "unexpected file name")))

(defun pam--card-score (score &optional already-done actual-answer)
  (let ((card-file (file-name-nondirectory (buffer-file-name)))
        stats
        new-interval)
    (save-excursion
      (pam-card-insert-score score actual-answer)
      (setq stats (pam-card-read-stats))
      (setq stats (pam-sm2 stats score))
      (pam-card-insert-stats stats)
      (setq new-interval (nth 1 stats))
      (unless already-done
        (let* ((todo-entry (pam--todo-from-file card-file))
               str)
          (with-current-buffer (pam-todo-file)
            (goto-char (point-min))
            (when (search-forward card-file)
              (if (memq score '(4 5))
                  (progn
                    (beginning-of-line)
                    (if (looking-at "\\* \\(TODO\\|REVIEW\\)")
                        (replace-match "DONE" nil nil nil 1)
                      (error "unexpected")))
                (setq str (buffer-substring-no-properties
                           (+ 7 (line-beginning-position))
                           (1+ (line-end-position))))
                (delete-region
                 (line-beginning-position)
                 (1+ (line-end-position)))
                (goto-char (point-max))
                (insert "* REVIEW " str))
              (pam-save-buffer)))
          (with-current-buffer (pam-todo-file new-interval)
            (goto-char (point-min))
            (unless (search-forward todo-entry nil t)
              (goto-char (point-max))
              (insert todo-entry)
              (pam-save-buffer))
            (kill-buffer))))
      (pam-save-buffer))))

(defun pam-card-score-1 ()
  (interactive)
  (pam-card-score 0))

(defun pam-card-score-2 ()
  (interactive)
  (pam-card-score 3))

(defun pam-card-score-3 ()
  (interactive)
  (pam-card-score 5))

(defvar-local pam-card-answer-validate-p nil)

(defun pam-card-answer ()
  "Answer the current card.
Enter the answer at point, then press \".\" to validate."
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward "^\\* m$" nil t)
    (delete-region (point-min) (match-beginning 0)))
  (goto-char (point-min))
  (insert "* \n")
  (goto-char 3)
  (setq pam-card-answer-validate-p t))

(defvar pam-is-redo nil)

(defun pam-card-validate-maybe (&optional arg)
  (interactive "p")
  (if pam-card-answer-validate-p
      (let ((tans (save-excursion
                    (goto-char (point-max))
                    (re-search-backward "^\\*")
                    (beginning-of-line 2)
                    (buffer-substring-no-properties
                     (point)
                     (1- (point-max)))))
            (actual-answer (buffer-substring-no-properties
                            (+ (line-beginning-position) 2)
                            (line-end-position))))
        (delete-region (point-min)
                       (1+ (line-end-position)))
        (setq pam-card-answer-validate-p nil)
        (if (pam-equal actual-answer tans)
            (if (save-excursion
                  (goto-char (point-max))
                  (re-search-backward "^\\* ")
                  (overlays-in (point) (point-max)))
                (if pam-is-redo
                    (pam-card-score 4)
                  (pam-card-score 5))
              (pam-card-score 3))
          (pam-card-score 0 actual-answer)))
    (self-insert-command arg)))

;;* Equivalence testing
(defvar pam-equiv-hash (make-hash-table :test 'equal))

(defvar pam-equiv-classes '(("we" "wij")
                            ("je" "jij")
                            ("ze" "zij")
                            ("u" "jij")
                            ("dichtbij" "vlakbij")
                            ("test" "toets")))

(defun pam-make-equivalent (a b)
  (puthash a b pam-equiv-hash)
  (puthash b b pam-equiv-hash))

(dolist (c pam-equiv-classes)
  (pam-make-equivalent (car c) (cadr c)))

(defun pam-equal (sa sb)
  "Check if the answer SA matches the question SB.
When SB has multiple lines, SA may match one of them."
  (if (string-match-p "\n" sb)
      (let ((sbl (split-string sb "\n" t))
            res)
        (while (and (null res) (setq sb (pop sbl)))
          (setq res (pam-equal-single sa sb)))
        res)
    (pam-equal-single sa sb)))

(defun pam-equal-single (sa sb)
  "Check if SA matches SB."
  (let ((lista (pam-sloppy sa))
        (listb (pam-sloppy sb))
        (res t)
        a b
        ah)
    (while (and res lista)
      (setq a (pop lista))
      (setq b (pop listb))
      (unless (or (string= a b)
                  (and (setq ah (gethash a pam-equiv-hash))
                       (equal ah
                              (gethash b pam-equiv-hash))))
        (setq res nil)))
    (and res (null listb))))

(defun pam-sloppy (str)
  (mapcar #'downcase
          (split-string str "[.,?! ]" t)))

(defvar pam-alist
  '(("/home/oleh/Dropbox/org/wiki/dutch.org" . "/home/oleh/Dropbox/source/site-lisp/git/dutch.pam"))
  "Map a master file to the corresponding repository.
Otherwise, the repository will be in the same directory as the master file.")

(defvar pam-path "/home/oleh/Dropbox/source/site-lisp/git/dutch.pam")

;;* Schedule files
(defun pam-repo-directory (file)
  "Return the Git repository that corresponds to FILE."
  (or (cdr (assoc file pam-alist))
      (expand-file-name
       (concat
        (file-name-sans-extension
         (file-name-nondirectory
          file))
        ".pam/"))))

(defvar pam-new-cards-per-day 75)

(defun pam-schedule ()
  (let* ((cards (directory-files "cards" nil "org$"))
         (n-cards (length cards))
         (today-gre (calendar-current-date))
         (today-abs (calendar-absolute-from-gregorian today-gre))
         (n-days (/ n-cards pam-new-cards-per-day))
         (dates (mapcar #'calendar-gregorian-from-absolute
                        (number-sequence today-abs (+ today-abs n-days)))))
    (dolist (date dates)
      (let ((file-name (format "pam-%d-%02d-%02d.org"
                               (nth 2 date)
                               (nth 0 date)
                               (nth 1 date)))
            (items (hydra-multipop cards pam-new-cards-per-day)))
        (call-process-shell-command
         (format "echo -e '%s' >> %s"
                 (concat "#+STARTUP: nologdone\\n#+SEQ_TODO: TODO REVIEW | DONE(d)\\n"
                         (mapconcat (lambda (s)
                                      (format "* TODO [[file:cards/%s][%s]]" s
                                              (car (split-string s "-"))))
                                    items "\\n"))
                 file-name))))))

(defun pam-delete-card (file)
  (interactive (list (buffer-file-name)))
  (when (file-exists-p file)
    (delete-file file)
    (when (string= (buffer-file-name) file)
      (kill-buffer))
    (let* ((default-directory (locate-dominating-file file ".git"))
           (prev-file (file-name-nondirectory file))
           (prev-scheduled
            (split-string
             (shell-command-to-string
              (format "git grep %s" prev-file))
             "\n" t))
           (save-silently t))
      (dolist (prev prev-scheduled)
        (unless (string-match "\\`\\([^:]+\\):.*\\[\\[file:cards/\\(.*\\)\\]\\[.*\\]\\'" prev)
          (user-error "Bad scheduled item: %s" prev))
        (let ((sfile
               (expand-file-name
                (match-string 1 prev))))
          (with-temp-buffer
            (insert-file-contents sfile)
            (when (re-search-forward prev-file nil t)
              (delete-region (line-beginning-position)
                             (1+ (line-end-position))))
            (write-file sfile)))))))

(defvar pam-git-items nil)

(defun pam-cmd-to-list (cmd &optional directory)
  (let ((default-directory (or directory default-directory)))
    (split-string
     (shell-command-to-string cmd)
     "\n" t)))

(defun pam-cards (repo-dir)
  (pam-cmd-to-list
   "git ls-files cards/"
   repo-dir))

(defun pam-visited-cards (repo-dir)
  (pam-cmd-to-list
   "git grep --files-with-matches '^\\*\\* scores'"
   repo-dir))

(defun pam-unvisited-cards (repo-dir)
  (pam-cmd-to-list
   "git grep --files-without-match '^\\*\\* scores' | grep cards/"
   repo-dir))

(defun pam-pile (repo-dir)
  "Pile up all unvisited cards into a single file."
  (let ((unvisited-cards (pam-unvisited-cards repo-dir))
        (schedule-files (pam-cmd-to-list "git ls-files --full-name pam-*-[0-9][0-9].org"))
        (save-silently t))
    (dolist (sf schedule-files)
      (with-current-buffer (find-file (expand-file-name sf repo-dir))
        (dolist (card unvisited-cards)
          (goto-char (point-min))
          (while (search-forward card nil t)
            (delete-region (line-beginning-position) (1+ (line-end-position)))))
        (pam-save-buffer)
        (kill-buffer)))
    (with-current-buffer (find-file (expand-file-name "pampile.org" repo-dir))
      (delete-region (point-min) (point-max))
      (dolist (card unvisited-cards)
        (insert (pam--todo-from-file (file-name-nondirectory card))))
      (pam-save-buffer)
      (kill-buffer))))

(defun pam-pull (arg)
  (interactive "p")
  (let ((sched-file (file-name-nondirectory (buffer-file-name)))
        (save-silently t)
        cards)
    (unless (string-match "^pam-.*org$" sched-file)
      (user-error "pull only when in a schedule file"))
    (when (= arg 1)
      (setq arg (read-number "how many cards: ")))
    (setq arg (min 100 arg))
    (with-current-buffer (find-file "pampile.org")
      (goto-char (point-min))
      (end-of-line arg)
      (setq cards (pam-delete-region (point-min)
                                     (min (1+ (point))
                                          (point-max))))
      (pam-save-buffer)
      (kill-buffer))
    (pam-goto-schedule-part)
    (insert cards)
    (pam-save-buffer)))

(defun pam-goto-schedule-part ()
  (goto-char (point-min))
  (if (re-search-forward "^\\*" nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))))

(defun pam--recompute-git-cards (repo-dir)
  (setq pam-git-items (make-hash-table :test 'equal))
  (let ((git-files (pam-cards repo-dir)))
    (dolist (gf git-files)
      (if (string-match "\\`cards/\\([^-]+\\)-" gf)
          (puthash (match-string 1 gf) gf pam-git-items)
        (error "unexpected")))))

(defun pam--replace-card (_card-front _card-body repo-dir card-file prev-file)
  (let ((old-metadata
         (with-temp-buffer
           (insert-file-contents (expand-file-name prev-file repo-dir))
           (goto-char (point-min))
           (when (looking-at "\\* m$")
             (outline-end-of-subtree)
             (buffer-substring-no-properties
              (point-min)
              (1+ (point)))))))
    (delete-file (expand-file-name prev-file repo-dir))
    (let ((prev-scheduled
           (pam-cmd-to-list (format "git grep %s" prev-file) repo-dir))
          (save-silently t))
      (dolist (prev prev-scheduled)
        (unless (string-match "\\`\\([^:]+\\):.*\\[\\[file:cards/\\(.*\\)\\]\\[.*\\]\\'" prev)
          (user-error "Bad scheduled item: %s" prev))
        (let ((file
               (expand-file-name
                (match-string 1 prev)
                repo-dir))
              (entry (match-string 2 prev)))
          (with-temp-buffer
            (insert-file-contents file)
            (when (re-search-forward entry nil t)
              (replace-match (file-name-nondirectory card-file)))
            (write-file file)))))
    old-metadata))

(defun pam-update-card (card-front card-body repo-dir)
  (let* ((card-id (md5 card-front))
         (card-file (concat "cards/" card-id "-"
                            (md5 card-body)
                            ".org"))
         (prev-file
          (gethash card-id pam-git-items))
         (metadata))
    (cond ((null prev-file))
          ((string= card-file prev-file))
          (t
           (when (file-exists-p (expand-file-name prev-file repo-dir))
             (setq metadata (pam--replace-card
                             card-front card-body repo-dir card-file prev-file)))))
    (unless (file-exists-p (expand-file-name card-file repo-dir))
      (let* ((txt
              (replace-regexp-in-string
               "'" "'\\''"
               (format "%s* %s\\n%s"
                       (or metadata "* m\\n#+STARTUP: content\\n")
                       card-front
                       card-body)
               t t))
             (cmd (format "echo -e '%s' > %s"
                          txt (expand-file-name card-file repo-dir))))
        (if (= 0 (call-process-shell-command cmd))
            (cons (if metadata
                      'update
                    'new)
                  card-file)
          (error "Command failed: %s" cmd))))))

(defconst pam-card-source-regexp "^\\* .*:cards:")

(defun pam-sync ()
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Must be in `org-mode' file"))
  (let ((repo-dir
         (pam-repo-directory (buffer-file-name)))
        (repo-is-new nil)
        (make-backup-files nil))
    (if (file-exists-p repo-dir)
        (unless (file-directory-p repo-dir)
          (error "%s must be a directory" repo-dir))
      (make-directory repo-dir)
      (let ((default-directory repo-dir))
        (shell-command "git init")
        (make-directory "cards/"))
      (setq repo-is-new t))
    (pam--recompute-git-cards repo-dir)
    (let ((old-point (point))
          (processed-headings nil)
          (new-cards nil)
          (updated-cards nil))
      (goto-char (point-min))
      (unless (re-search-forward pam-card-source-regexp nil t)
        (error "No outlines with the :cards: tag found"))
      (beginning-of-line)
      (while (re-search-forward pam-card-source-regexp nil t)
        (lispy-destructuring-setq (processed-headings new-cards updated-cards)
            (pam-sync-current-outline
             processed-headings new-cards updated-cards repo-dir)))
      (goto-char old-point)
      (when (or new-cards updated-cards)
        (pam-schedule-today
         (mapcar #'pam--todo-from-file new-cards)
         (find-file (expand-file-name "pampile.org" repo-dir)))
        (when repo-is-new
          nil)
        (shell-command-to-string
         (format
          "cd %s && git add . && git commit -m %s"
          repo-dir
          (cond ((null updated-cards)
                 (format "'Add %d new card(s)'" (length new-cards)))
                ((null new-cards)
                 (format "'Update %d card(s)'" (length updated-cards)))
                (t
                 (format "'Add %d new card(s), update %d cards'"
                         (length new-cards)
                         (length updated-cards)))))))
      (message "%d new cards, %d updated, %d total"
               (length new-cards)
               (length updated-cards)
               (length processed-headings)))))

(defun pam-sync-current-outline (processed-headings new-cards updated-cards repo-dir)
  (let ((end (save-excursion
               (outline-end-of-subtree)
               (skip-chars-backward "\n ")
               (point))))
    (while (re-search-forward "^\\*\\{2,3\\} \\(.*\\)$" end t)
      (let* ((card-front (match-string-no-properties 1))
             (card-body (buffer-substring-no-properties
                         (1+ (point))
                         (if (re-search-forward "^\\*" end t)
                             (progn
                               (backward-char 2)
                               (point))
                           end)))
             card-info
             card-file)
        (if (member card-front processed-headings)
            (error "Duplicate heading encountered: %s" card-front)
          (push card-front processed-headings))
        (when (setq card-info (pam-update-card card-front card-body repo-dir))
          (setq card-file (file-name-nondirectory (cdr card-info)))
          (cond ((eq (car card-info) 'new)
                 (push card-file new-cards))
                ((eq (car card-info) 'update)
                 (push card-file updated-cards))))))
    (list processed-headings new-cards updated-cards)))

(defun pam-default-directory ()
  (if (string-match "^\\(.*\\.pam/\\)" default-directory)
      (expand-file-name (match-string 1 default-directory))
    pam-path))

(defun pam-kill-buffers ()
  (let* ((pdir (pam-default-directory))
         (cards-dir (expand-file-name "cards/" pdir)))
    (dolist (b (buffer-list))
      (when (buffer-file-name b)
        (let ((dir (file-name-directory (buffer-file-name b))))
          (when (or (equal dir cards-dir)
                    (and (equal dir pdir)
                         (not (equal (file-name-nondirectory
                                      (buffer-file-name b))
                                     (pam-schedule-file (current-time))))))
            (kill-buffer b)))))))

(defun pam-schedule-file (time)
  (format-time-string "pam-%Y-%m-%d.org" time))

(defun pam-todo-file (&optional offset)
  (setq offset (or offset 0))
  (let ((default-directory (pam-default-directory))
        (todo-file (pam-schedule-file
                    (time-add
                     (current-time)
                     (days-to-time offset))))
        (save-silently t))
    (when (and (eq offset 0) (not (file-exists-p todo-file)))
      (find-file-literally todo-file)
      (insert "#+STARTUP: nologdone\n#+SEQ_TODO: TODO REVIEW | DONE(d)\n")
      (pam-pull 10)
      (pam-save-buffer)
      (message "Schedule was empty, used `pam-pull' for 10 cards"))
    (find-file-noselect todo-file)))

(defvar pam-last-rechedule nil)

(defun pam-schedule-today (cards &optional buffer)
  (with-current-buffer (or buffer (pam-todo-file))
    (pam-goto-schedule-part)
    (dolist (card cards)
      (insert card))
    (let ((save-silently t))
      (pam-save-buffer))))

(defun pam-reschedule-maybe ()
  (let ((today (calendar-current-date)))
    (unless (and pam-last-rechedule
                 (<
                  (calendar-absolute-from-gregorian today)
                  (calendar-absolute-from-gregorian pam-last-rechedule)))
      (setq pam-last-rechedule today)
      (let* ((today-file (pam-todo-file))
             (today-file-name (file-name-nondirectory
                               (buffer-file-name today-file)))
             (pdir (pam-default-directory))
             (all-files (directory-files pdir nil "org$"))
             (idx (cl-position today-file-name all-files
                               :test 'equal))
             (old-files (reverse (cl-subseq all-files 0 idx))))
        (dolist (old-file old-files)
          (setq old-file (expand-file-name old-file pdir))
          (let (cards)
            (with-current-buffer (find-file-noselect old-file)
              (goto-char (point-min))
              (while (re-search-forward "^\\* \\(TODO\\|REVIEW\\) " nil t)
                (push (buffer-substring-no-properties
                       (point) (1+ (line-end-position)))
                      cards)))
            (pam-schedule-today (mapcar (lambda (s) (concat "* TODO " s))
                                        (nreverse cards)))
            (delete-file old-file)))))))

;;;###autoload
(defun pam-drill ()
  (interactive)
  (pam-reschedule-maybe)
  (let (card-link card-file)
    (when (bound-and-true-p pam-card-mode)
      (when (buffer-modified-p)
        (pam-save-buffer))
      (kill-buffer))
    (delete-other-windows)
    (split-window-horizontally)
    (pam-kill-buffers)
    (switch-to-buffer (pam-todo-file))
    (goto-char (point-min))
    (when (re-search-forward "^* \\(TODO\\|REVIEW\\) " nil t)
      (recenter 5)
      (setq card-link (buffer-substring-no-properties
                       (point) (line-end-position)))
      (beginning-of-line)
      (set-window-point (selected-window) (point)))
    (other-window 1)
    (if (null card-link)
        (message "%d cards learned/reviewed today. Well done!"
                 (cl-count-if
                  (lambda (x) (string-match "^\\* DONE" x))
                  (split-string (with-current-buffer (pam-todo-file)
                                  (buffer-string)) "\n")))
      (unless (string-match "\\`\\[\\[file:\\([^]]+\\)\\]\\[.*\\]\\]\\'" card-link)
        (error "Bad entry in %s: %s" (pam-todo-file) card-link))
      (setq card-file (match-string 1 card-link))
      (switch-to-buffer
       (find-file-noselect
        (expand-file-name card-file (pam-default-directory))))
      (pam-card-mode))))

(defun pam-commit ()
  (interactive)
  (let* ((default-directory (locate-dominating-file (or (buffer-file-name)
                                                        default-directory) ".git"))
         (card-count
          (cl-count-if
           (lambda (s) (string-match "modified.*cards/" s))
           (pam-cmd-to-list "git status")))
         (card-str (if (= card-count 1)
                       "card"
                     "cards")))
    (message
     (shell-command-to-string
      (format
       "git add . && git commit -m 'Do %s %s'"
       card-count card-str)))))

(defun pam-unschedule-card (card-file)
  "Unschedule CARD-FILE everywhere and schedule it for today."
  (let* ((repo-dir (locate-dominating-file card-file ".git"))
         (s-files (pam-cmd-to-list (format "git grep --files-with-matches %s" card-file)
                                   repo-dir)))
    (dolist (file s-files)
      (with-current-buffer (find-file-noselect (expand-file-name file repo-dir))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward card-file nil t)
            (delete-region (line-beginning-position)
                           (1+ (line-end-position))))
          (let ((save-silently t))
            (pam-save-buffer)))
        (unless (equal (current-buffer) (pam-todo-file))
          (kill-buffer))))
    (with-current-buffer (pam-todo-file)
      (pam-goto-schedule-part)
      (if (re-search-forward "^\\* \\(TODO\\|REVIEW\\)" nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max)))
      (insert (pam--todo-from-file card-file)))))

(defun pam-redo ()
  "Redo the current card without penalty."
  (interactive)
  (if (string-match-p "cards/.*org\\'" (buffer-file-name))
      (let ((fname (buffer-file-name)))
        (pam-save-buffer)
        (pam-cmd-to-list (format "git checkout -- %s" fname))
        (revert-buffer nil t nil)
        (pam-unschedule-card (file-name-nondirectory fname))
        (setq-local pam-is-redo t)
        (pam-card-mode))
    (user-error "applies only to card files")))

;;* `pam-card-mode'
(defvar pam-card-mode-map
  (let ((map (make-sparse-keymap)))
    (worf-define-key map (kbd "q") 'bury-buffer)
    (worf-define-key map (kbd "a") 'pam-card-answer
                     :break t)
    (worf-define-key map (kbd "R") 'pam-redo
                     :break t)
    (worf-define-key map (kbd "C") 'pam-commit)
    (worf-define-key map (kbd "r") 'org-cycle)
    (worf-define-key map (kbd "1") 'pam-card-score-1)
    (worf-define-key map (kbd "2") 'pam-card-score-2)
    (worf-define-key map (kbd "3") 'pam-card-score-3)
    (worf-define-key map (kbd "n") 'pam-drill
                     :break t)
    (define-key map (kbd ".") 'pam-card-validate-maybe)
    map))

(define-minor-mode pam-card-mode
  "Minor mode for Pam cards.

\\{pam-card-mode-map}"
  :lighter " p"
  (when pam-card-mode
    (if (eq major-mode 'org-mode)
        (progn
          (setq org-cycle-global-status 'contents)
          (goto-char (point-min))
          (pam-card-answer))
      (pam-card-mode -1))))

(lispy-raise-minor-mode 'pam-card-mode)

(provide 'pamparam)

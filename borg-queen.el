;;; borg-queen -- Manage, install and upgrade Borg drones.

;; Copyright (c) 2017 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Homepage: https://github.com/thblt/borg-queen
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see https://www.gnu.org/licenses.

;;; Commentary:

;; Manage, maintain and upgrade Borg packages.

;;; Code:

(require 'borg)
(require 'dash)
(require 'magit)
(require 'tabulated-list)

;;; Options

(defgroup borg-queen nil
  "Manage and update Borg drones.")

(defgroup borg-queen-faces nil
  "Faces for Borg Queen."
  :group 'borg-queen)

(defcustom borg-queen-pgp-verify
  nil
  "If t, no package will be updated without a valid PGP signature."
  :group 'borg-queen)

(defcustom borg-queen-pgp-global-keys
  nil
  "A list of PGP keys IDs unconditionally accepted to sign all packages.

This should be either nil or your own key."
  :group 'borg-queen)

(defcustom borg-queen-upgrade-strategy
  'auto
  "Determine whether to upgrade to tags or commits.

Value can be 'tag, 'head or 'auto.

'tag (roughly) mimics the behavior of using Melpa Stable.  'head
will always auto-upgrade to HEAD.  'auto is equivalent to 'tag if
the repository contains at least one named tag, and 'head
otherwise.

This property can be overriden for specific drones with `borg-queen-drone-upgrade-strategies'"
  :group 'borg-queen)

(defcustom borg-queen-required-drones
  nil
  "A list of required packages.  This is used by
  `borg-queen-why'."
  :group 'borg-queen)

(defcustom borg-queen-drone-upgrade-strategies
  nil
  "An alist of (DRONE-NAME UPGRADE-STRATEGY).  DRONE-NAME is a
  string, UPGRADE-STRATEGY takes the same values as in
  `borg-queen-upgrade-strategy'"
  :group 'borg-queen)

(defcustom borg-queen-mark-delimiter
  " ▸ "
  "@TODO")

(defcustom borg-queen-marks-reprs
  `((borg-queen--checkout-action . ,(propertize "Checkout %s" 'face 'borg-queen-checkout-mark-face))
    (borg-queen--remove-action . ,(propertize "REMOVE"  'face 'borg-queen-remove-mark-face))
    (borg-queen--assimilate-action . ,(propertize "Assimilate"  'face 'borg-queen-assimilate-mark-face)))
  "@TODO")

(defcustom borg-queen-state-ok-symbol
  "✓"
  "Symbol to use if this package has no issues.")

(defcustom borg-queen-state-ok-but-no-pgp-symbol
  " "
  "Symbol to use if this package has no issues but has not enabled PGP signature verification.")

(defcustom borg-queen-state-warning-symbol
  "?"
  "Symbol to use if this package has minor issues.")

(defcustom borg-queen-state-error-symbol
  "!"
  "Symbol to use if this package has severe issues.")

;;; Variables

(defvar borg-queen-dependencies (make-hash-table)
  "An alist of (DRONE . DEPS).

DRONE is the drone's name as a string (ie, the submodule path
without lib/).

DEPS is either T if the drone is directly required, or a list of
drone names that depend on this drone." )

;;; Faces

(defface borg-queen-state-ok-face
  '((t :foreground "LawnGreen"))
  "State for the OK state symbol"
  :group 'borg-queen-faces)

(defface borg-queen-state-ok-but-no-pgp-face
  '((t :inherit shadow))
  "State for the OK state symbol"
  :group 'borg-queen-faces)

(defface borg-queen-state-warning-face
  '((t :foreground "orange"))
  "State for the warning state symbol"
  :group 'borg-queen-faces)

(defface borg-queen-state-error-face
  '((t :background "red" :foreground "white"))
  "State for the error state symbol"
  :group 'borg-queen-faces)

(defface borg-queen-package-name-face
  '((t :inherit font-lock-comment-face))
  "Face for package names"
  :group 'borg-queen-faces)

(defface borg-queen-package-name-required-face
  '((t :inherit font-lock-keyword-face))
  "Face for required package names"
  :group 'borg-queen-faces)

(defface borg-queen-type-clone-face
  '((t :inherit bold))
  "Face for Type column when type is Clone."
  :group 'borg-queen-faces)

(defface borg-queen-type-drone-face
  '((t :inherit shadow))
  "Face for Type column when type is Drone."
  :group 'borg-queen-faces)

(defface borg-queen-upgrade-strategy-face
  '((t :inherit default))
  "Face for the upgrade strategy."
  :group 'borg-queen-faces)

(defface borg-queen-available-tag-face
  '((t :distant-foreground "MediumBlue" :foreground "DeepSkyBlue"))
  "Face for Type column when type is Drone."
  :group 'borg-queen-faces)

(defface borg-queen-mark-delimiter-face
  '((t :inherit 'bold))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-checkout-mark-face
  '((t :distant-foreground "black" :foreground "green" :inverse-video t))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-remove-mark-face
  '((t :distant-foreground "orange" :foreground "red" :inverse-video t))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-assimilate-mark-face
  '((t :distant-foreground "MediumBlue" :foreground "DeepSkyBlue" :inverse-video t))
  "@TODO"
  :group 'borg-queen-faces)

;;; Mode

(defvar borg-queen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'borg-queen-mark-for-checkout-auto)
    (define-key map (kbd "H") 'borg-queen-mark-for-checkout-head)
    (define-key map (kbd "c") 'borg-queen-mark-for-checkout-commit)
    (define-key map (kbd "t") 'borg-queen-mark-for-checkout-tag)
    (define-key map (kbd "T") 'borg-queen-mark-for-checkout-any-tag)

    (define-key map (kbd "A") 'borg-queen-mark-for-assimilation)
    (define-key map (kbd "R") 'borg-queen-mark-for-removal)

    (define-key map (kbd "-") 'borg-queen-unmark)

    (define-key map (kbd "x") 'borg-queen-run-marks)

    (define-key map (kbd "a") 'borg-queen-assimilate)
    (define-key map (kbd "d") 'borg-queen-describe)
    (define-key map (kbd "f") 'borg-queen-fork)

    (define-key map (kbd "?") 'borg-queen-show-issues)

    (define-key map (kbd "f") 'borg-queen-commit)

    (define-key map (kbd "b") 'borg-queen-clone)
    (define-key map (kbd "w") 'borg-queen-why)

    (define-key map (kbd "g") 'borg-queen)
    (define-key map (kbd "G") 'borg-queen-fetch-and-refresh)
    map)
  "Keymap for Borg Queen mode.")

(defvar-local borg-queen--state
  nil
  "The state of the Collective, for use by its Queen.

For a more formal documentation of this variable, see
documentation for function `borg-queen--state'.")

(defvar-local borg-queen--marks
  nil
  "An alist of (drone mark)")

(define-derived-mode borg-queen-mode tabulated-list-mode "Borg Queen"
  "Major mode for the Borg Queen."
  :group 'borg-queen
  (setq tabulated-list-format `[ ("S" 1 t)
                                 ("Package" ,(+ 20 (-max (mapcar 'length (borg-clones)))) t)
                                 ;; ("Mark" 16 t)
                                 ("Type" 5 t)
                                 ("Upd" 6 t)
                                 ("Installed" 20 t)
                                 ("Commits" 7 nil . (:right-align t))
                                 ("Available" 12 t)
                                 ("Signature" 99 t) ]
        tabulated-list-sort-key (cons "Package" nil)
        tabulated-list-padding 1)
  (tabulated-list-init-header)
  (hl-line-mode))

;;;###autoload
(defun borg-queen ()
  "Manage and update Borg drones."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Borg Queen*"))
  (borg-queen-mode)
  (setq borg-queen--state (borg-queen--state))
  (setq tabulated-list-entries 'borg-queen--tabulated-list-entries)
  (tabulated-list-print t)
  (message "Borg Queen is ready."))

(defun borg-queen--tabulated-list-entries ()
  "Build the list of tabulated list entries."
  (mapcar (lambda (item)
            (let* ((name (car item))
                   (state (cdr item))
                   (required (member item borg-queen-required-drones))
                   (mark (cdr (assoc name borg-queen--marks)))
                   )
              `(,name
                [
                 ;; Status symbol
                 ,(cond ((plist-get state :errors)
                         (propertize borg-queen-state-error-symbol 'face 'borg-queen-state-error-face))
                        ((plist-get state :warnings)
                         (propertize borg-queen-state-warning-symbol 'face 'borg-queen-state-warning-face))
                        ((lax-plist-get state :signatures)
                         (propertize borg-queen-state-ok-symbol 'face 'borg-queen-state-ok-face))
                        (t
                         (propertize borg-queen-state-ok-but-no-pgp-symbol 'face 'borg-queen-state-ok-but-no-pgp-face)))

                 ;; Name
                 ,(concat
                   (propertize name 'face (if (and (not required)
                                                   borg-queen-required-drones)
                                              'borg-queen-package-name-face
                                            'borg-queen-package-name-required-face))

                   (if mark
                       (concat
                        (propertize borg-queen-mark-delimiter 'face 'borg-queen-mark-delimiter-face)
                        (format
                         (alist-get (car mark) borg-queen-marks-reprs)
                         (cadr mark)))
                     " "))

                 ;; Type
                 ,(if (plist-get state :assimilated)
                      (propertize "Drone" 'face 'borg-queen-type-drone-face)
                    (propertize "Clone" 'face 'borg-queen-type-clone-face))

                 ;; Upgrade strategy
                 ,(let ((strategy (borg-queen-get-upgrade-strategy name)))
                    (propertize
                     (cond ((equal strategy 'tag) "Tag")
                           ((equal strategy 'head) "Hea")
                           ((equal strategy 'auto) "Aut"))
                     'face (if (equal strategy borg-queen-upgrade-strategy)
                               'shadow
                             'default)))

                 ;; Installed version
                 ,(lax-plist-get state :version)

                 ;; New commits
                 ,(let ((count (lax-plist-get state :new-commits)))
                    (propertize (number-to-string count)
                                'face (if (> count 0) 'default 'shadow)))

                 ;; Available
                 ,(-if-let* ((latest-tag (lax-plist-get state :latest-tag)))
                      (if (not (equal latest-tag (lax-plist-get state :version-on-tag)))
                          (propertize latest-tag 'face 'borg-queen-available-tag-face)
                        "")
                    "")

                 ;; Signature
                 ,(--if-let (lax-plist-get state :signatures)
                      (format "%s (%s)" (cadar it) (caar it))
                    "")
                 ]
                )))
          borg-queen--state))

(defun borg-queen-get-upgrade-strategy (drone)
  "Return the upgrade strategy for drone.
@FIXME"
  borg-queen-upgrade-strategy)

(defun borg-queen--state ()
  "Return the state of the Collective.

The returned value is an alist of plist.  The keys of the alist
are package names, as strings.  The keys of each plist are as
follows:

 `:name': drone name as a string.

 `:version': either the output of `git describe' or the short hash
 of the current commit (if `version-type' == 2).

 `:version-type': 0 if the current commit is exactly a tag, 1 if
 it's a tag + a number of changes, 2 if it's a commit without a
 previous tag.

 `:version-on-tag': if `version-type' is 0, then this is equal to
 `version'.  If `version-type' is 1, it contains the name of the
 most recent tag this commit is based on.  Otherwise, its value
 is nil.

 `:assimilated': t if DRONE is registered as a submodule.

 `:new-commits': the number of new commits.

 `:tags': the list of tags.

 `:latest-tag': the most recent tag."
  (message "Gathering drones state...")
  (let ((borg-drones (borg-drones)))
    (mapcar (lambda (drone)
              (-flatten-n
               1
               (let* ((default-directory (expand-file-name drone borg-drone-directory))
                      ;; Tags
                      (tags (magit-git-lines "tag"  "--sort" "creatordate"  "--merged" "origin/master"))
                      ;; @FIXME Don't assume origin branch name.                               ^^^^^^

                      ;; Version
                      (version-tag (magit-git-lines "describe" "--tags" "--exact-match"))
                      (version-tag-commits  (magit-git-lines "describe" "--tags"))
                      (version-commit (magit-git-lines "describe" "--tags" "--always"))
                      (version (car (or version-tag version-tag-commits version-commit)))

                      (version-type (cond
                                     (version-tag 0)
                                     (version-tag-commits 1)
                                     (version-commit 2)))

                      (version-on-tag (car (magit-git-lines "describe" "--tags" "--abbrev=0")))

                      ;; Signatures
                      (signatures (or
                                   (when (= version-type 0)
                                     (borg-queen-gpg-verify drone))
                                   (borg-queen-gpg-verify drone))))

                 `(,drone

                   ;; :version
                   (:version ,version)

                   (:version-type ,version-type)

                   ,(when version-on-tag
                      `(:version-on-tag ,version-on-tag))

                   ;; :latest-tag
                   ,(when tags
                      `(:latest-tag ,(-last-item tags)))

                   ;; :assimilated
                   ,(when (member drone borg-drones)
                      '(:assimilated t))

                   ;; :new-commits
                   (:new-commits ,(length (magit-git-lines "log" "--format=oneline" "HEAD..origin")))

                   ;; tags
                   (:tags ,tags)

                   ;; numeric signature
                   ,(when signatures
                      `(:signatures ,signatures))

                   ;; warnings
                   ,(when (and
                           (equal 'tags (borg-queen-get-upgrade-strategy drone))
                           (not tags))
                      `(:warnings ("No tags in this repository, yet it is configured to update on tags only.")))

                   ;; errors
                   ,(when (and (borg-queen-gpg-require-signature drone)
                               (null signatures))
                      `(:errors ("Missing or invalid signature!")))
                   ))))
            (borg-clones))))


;;;; Mark

(defmacro borg-queen--with-selection (&rest body)
  "Execute BODY with each selected entry, binding DRONE to its name."
  `(mapc (lambda (drone) ,@body)
         (if mark-active
             nil ;; @TODO Build list of selected items
           (list (tabulated-list-get-id)))))

(defun borg-queen--mark (drone &rest mark)
  "Add MARK to DRONE.

DRONE is a drone or clone name as a string.

MARK is nil, or a list whose car is a function name, and cdr its
arguments except the drone name.

If DRONE already has a mark, it is replaced."
  (if mark
      (map-put borg-queen--marks drone mark)
    (map-delete borg-queen--marks drone))
  (tabulated-list-revert t))

(defun borg-queen-mark-for-checkout-auto ()
  "@TODO"
  (interactive)
  (borg-queen--with-selection
   (let ((strategy (borg-queen-get-upgrade-strategy drone)))
     (cond ((equal strategy 'tag)
            (borg-queen-mark-for-checkout-latest-tag))

           ((equal strategy 'head)
            (borg-queen-mark-for-checkout-head))

           ((equal strategy 'auto)
            (if (borg-queen--aplist-get drone :tags borg-queen--state)
                (borg-queen-mark-for-checkout-latest-tag)
              (borg-queen-mark-for-checkout-head)))

           (t (error (format "Unknow strategy %s" strategy)))))))

(defun borg-queen-mark-for-checkout-latest-tag ()
  "Mark selection to checkout the most recent tag."
  (interactive)
  (borg-queen--with-selection
   (borg-queen--mark-for-checkout
    drone
    (borg-queen--aplist-get drone :latest-tag borg-queen--state)
    t)))

   (defun borg-queen-mark-for-checkout-head ()
  "Mark for checkout origin/HEAD"
  (interactive)
  (borg-queen--with-selection
   (borg-queen--mark-for-checkout drone "origin/HEAD" nil)))

(defun borg-queen-mark-for-checkout-commit ()
  "Prompt for commit then mark it for checkout."
  (interactive)
  (borg-queen--with-selection
   (let* ((default-directory (expand-file-name drone borg-drone-directory))
          (commit (completing-read
                   (format "Checkout %s to commit: " drone)
                   (magit-git-lines "log" "--format=%h %s" "origin/HEAD")))))))

(defun borg-queen--mark-for-checkout (drone object is-tag)
  "Mark DRONE to checkout to OBJECT.

If IS-TAG, OBJECT is a tag name, otherwise a commit hash.

This functions runs a cryptographic verification on the target
object and checks it returns a valid key before going on.  If the
current commit (HEAD) has a valid signature and the target OBJECT
hasn't, it warns the user before marking with `yes-or-no-p'."
  (when (or
         (borg-queen-gpg-verify drone object is-tag)
         (not (borg-queen--aplist-get drone :signatures borg-queen--state))
         (yes-or-no-p
          (format "Missing or invalid signature for %s %s on drone %s.  Mark anyway?"
                  (if is-tag "tag" "commit")
                  object
                  drone)))
    (borg-queen--mark drone 'borg-queen--checkout-action object is-tag)))


(defun borg-queen-mark-for-assimilation ()
  "@TODO"
  (interactive)
  (borg-queen--with-selection
   (if (borg-queen--aplist-get drone :assimilated borg-queen--state)
       (message "Already assimilated!")
     (borg-queen--mark drone 'borg-queen--assimilate-action))))

(defun borg-queen-mark-for-removal ()
  "Mark DRONE for removal."
  (interactive)
  (borg-queen--with-selection
   (when (y-or-n-p (format "Mark %s for removal? " drone))
     (borg-queen--mark drone 'borg-queen--remove-action))))

(defun borg-queen-unmark ()
  "@TODO"
  (interactive)
  (borg-queen--with-selection
   (borg-queen--mark drone)))

(defun borg-queen-run-marks ()
  "Execute marks."
  (interactive)
  (when (and borg-queen--marks
             (y-or-n-p (format "Execute %s mark(s)?" (length borg-queen--marks))))
    (dolist (mark borg-queen--marks)
      (eval (-flatten `(,(cadr mark)
                        ,(car mark)
                        ,(cddr mark)))))))

;;; Other commands

(defun borg-queen-assimilate () "@TODO" (interactive))
(defun borg-queen-clone () "@TODO" (interactive))

(defun borg-queen-describe ()
  "Describe package at point."
  (interactive)
  (epkg-describe-package (tabulated-list-get-id)))

(defun borg-queen-why ()
  "Show a dependency path from package at point to a required package."
  (interactive)
  (message "Unimplemented")) ;; @TODO.

(defun borg-queen-fork () "@TODO" (interactive))

(defun borg-queen-show-issues ()
  "Print issues on package at point."
  (interactive)
  (let* ((drone (tabulated-list-get-id))
         (state (cdr (assoc drone borg-queen--state)))
         (warnings (lax-plist-get state :warnings))
         (errors (lax-plist-get state :errors)))
    (if (or warnings errors)
        (with-output-to-temp-buffer "*Package Issues*"
          (princ
           (format "Package %s has %s warning(s) and %s error(s).\n" drone (length warnings) (length errors)))
          (dolist (e errors)
            (princ (format "\n\tError: %s" e)))
          (dolist (w warnings)
            (princ (format "\n\tWarning: %s " w))))
      (message "No issues for %s." drone))))


(defun borg-queen-commit () "@TODO" (interactive))
(defun borg-queen-fetch-and-refresh () "@TODO" (interactive))

(defun borg-queen--checkout-action (drone object &optional tag)
  "Checkout DRONE to git object OBJECT.  If TAG is non-nil,
OBJECT is understood as a TAG name."
  (message "Checkout %s to %s" drone version))

(defun borg-queen--remove-action (drone)
  "Remove DRONE."
  )

(defun borg-queen--assimilate-action (drone)
  "Assimilate DRONE."
  (message (format "Assimilate!!! %s" drone))
  )


(defun borg-queen--aplist-get (akey pkey aplist)
  "Return the value associated to PKEY in a plist associated to AKEY in APLIST."
  (lax-plist-get (cdr (assoc akey aplist)) pkey))


;;; GPG

(defun borg-queen-gpg-verify (drone &optional object is-tag)
  "PGP-verify DRONE commit or tag OBJECT.

if IS-TAG is non-nil, OBJECT is treated as a commit hash,
otherwise as a tag name.

If object is not provided, it defaults to \"HEAD\"."

  (unless object
    (setq object "HEAD"))

  ;; @FIXME: Add fallback.  If IS-TAG, find the commit this tag points
  ;; to, and verify the commit if the tag isn't signed.  Other, find
  ;; tags pointing to OBJECT and verify them if commit isn't signed.

  (let* ((default-directory (expand-file-name drone borg-drone-directory))
         (commands (cons
                    ;; First chance: verify the provided object.
                    (format "git verify-%s --raw %s"
                          (if is-tag "tag" "commit")
                          (shell-quote-argument object))

                    ;; Second chance: verify tag if commit, commit if tag
                    (if is-tag
                        ;; Verify the commit this tag points to.
                        (list (format "git verify-commit --raw %s"
                                      (shell-quote-argument (car (magit-git-lines "rev-list" "-n" "1" object)))))

                      ;; Verify the tag(s) pointing to this commit.
                      (mapcar (lambda (tag) (format "git verify-tag --raw %s" (shell-quote-argument tag)))
                            (magit-git-lines "tag" "--points-at" object)))))

         (signatures (borg-queen--gpg-parse-verify-output
                      (split-string
                       (mapconcat 'shell-command-to-string commands "\n")
                       (rx "\n")))))

    (when (borg-queen--gpg-match-keys
           (mapcar 'car signatures)
           (append (borg-get-all drone "signingkey") borg-queen-pgp-global-keys))
      signatures)))

(defun borg-queen-gpg-require-signature (drone)
  "Return t if DRONE is configured to require a valid signature."
  (or
   borg-queen-pgp-verify
   (borg-get-all drone "signingkey")))

(defun borg-queen--gpg-parse-verify-output (lines)
  "Parse LINES as output of git verify-[tag,commit] --raw ...

Returns nil or a list of (KEYID OWNERID)."
  (-non-nil
   (mapcar (lambda (str)
             ;; @FIXME Verify.
             ;; The regexp below is based on the actual output of the
             ;; program and on documentation found here:
             ;; https://www.gnupg.org/documentation/manuals/gnupg/Automated-signature-checking.html
             (when (string-match (rx
                                  line-start
                                  "[GNUPG:] GOODSIG "
                                  (group (one-or-more hex-digit))
                                  " "
                                  (group (one-or-more any))
                                  line-end)
                                 str)
               `(,(match-string 1 str)
                 ,(match-string 2 str))))
           lines)))

(defun borg-queen--gpg-collect-fingerprints (keys)
  "Run gpg --list-keys --with-colons KEYS, and return a list of fingerprints.

KEYS is a list of string."
  (let ((fprs nil))
    (with-temp-buffer
      (apply #'call-process epg-gpg-program nil t nil
             "--list-keys" "--with-colons" "--" keys)
      (goto-char (point-min))
      (while (re-search-forward "^pub" nil t)
        (when (re-search-forward "^fpr" nil t)
          (push (nth 9 (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))
                                     ":"))
                fprs)))
      fprs)))

(defun borg-queen--gpg-match-keys (keys other-keys)
  "Return non-nil iff KEYS and OTHER-KEYS intersect.

KEYS and OTHER-KEYS are list of strings, which should be valid
GnuPG key identifiers.  The return value is the
intersection (list of key fingerprint strings) of the keys or
nil"
  (cl-intersection (borg-queen--gpg-collect-fingerprints keys)
                   (borg-queen--gpg-collect-fingerprints other-keys)
                   :test #'equalp))

;;; Dependency graph

(defmacro want-drone (&rest d)
  "Declare required drones and their dependencies.

This macro takes a list of symbols D and treats it as a list of
drone names, optionnally followed by the list of their
dependencies.  For example:

(want-drone borg-queen (borg))

Multiple drones may be declared in a single declaration:

(want-drone drone1 (depA depB)
            drone2
            drone3 (depC depB))

For that purpose, this macro is aliased to `want-drones'.

At some point, the Queen will use this to identify orphans."
(when d
  (let ((drone (symbol-name (car d)))
        (deps (cadr d)))
    `(progn
     (puthash ,drone t borg-queen-dependencies)
     ,(when (listp deps)
        `(mapc (lambda (d) (puthash (symbol-name d) (list ,drone) borg-queen-dependencies)) ,deps))

     ,(-flatten-n 1 `(want-drone ,(funcall (if (listp deps) 'cddr 'cdr) d)))))
         )
       )

(defalias 'want-drones 'want-drone)

(provide 'borg-queen)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; borg-queen.el ends here

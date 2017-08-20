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
  'commits
  "Determine whether to upgrade to tags or commits.

If 'tag, packages will be auto upgraded to the tag describing the
highest version number.  If 'commit or any other value, auto
upgrade will go the most recent commit.

'tag (roughly) mimics the behavior of using Melpa Stable.

This property can be overriden for specific drones with `borg-queen-drone-properties'"
  :group 'borg-queen)

(defcustom borg-queen-drones-properties
  '(("borg" :required t))
  "An alist of plists of drones properties for use with Borg Queen.

Entries are of them form (DRONE PROPERTIES), where PROPERTIES is
  either nil or a plist with the following entries:

  `:required' -- t if the package is explicitely required by your
  configuration.  This will be used by the upcoming dependency
  tracker to identify orphan drones.  (Default: nil)

  `:upgrade-remote' -- the name of the remote to upgrade from.
  (Default: upstream if set, or the first remote, if not.)

  `:upgrade-strategy' -- either 'tag or 'commit (Default:
  `borg-queen-upgrade-strategy`, see this variable for details)

  `:pgp-verify' -- Either t to accept valid signatures from any
  key present in gpg keyring, or a key identifier or a list of
  hex key identifiers in any form gpg will understand.
  Eg: (\"68CD0D65971CD850C96AA335D10A081695D25BA7\",
  \"33CDE511\")

  This property has _LOWER_ priority than `:pgp-no-verify'.

  `:pgp-no-verify' -- t to locally override
  `borg-queen-pgp-always-verify'.  This has _HIGHER_ priority
  than `:pgp-verify'."
  :group 'borg-queen)

(defcustom borg-queen-marks-reprs
  `((borg-queen--upgrade-action . ,(propertize " ▲ %s " 'face 'borg-queen-upgrade-mark-face))
    (borg-queen--downgrade-action . ,(propertize " ▼ %s " 'face 'borg-queen-downgrade-mark-face))
    (borg-queen--remove-action . ,(propertize " ❌ REMOVE "  'face 'borg-queen-remove-mark-face))
    (borg-queen--assimilate-action . ,(propertize " ⦿ ASSIMILATE "  'face 'borg-queen-assimilate-mark-face)))
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
  '((t :background "red" :foreground  "white"))
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

;; Marks

(defface borg-queen-upgrade-mark-face
  '((t :foreground "black" :distant-foreground "#00CC00" :background "#00CC00" :weight bold))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-downgrade-mark-face
  '((t :foreground "white" :foreground "#0000CC" :background "#0000CC" :weight bold))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-remove-mark-face
  '((t :foreground "white" :distant-foreground "red" :background "red" :weight bold))
  "@TODO"
  :group 'borg-queen-faces)

(defface borg-queen-assimilate-mark-face
  '((t :foreground "black" :distant-foreground "white" :background "white" :weight bold))
  "@TODO"
  :group 'borg-queen-faces)

(defvar borg-queen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'borg-queen-mark-for-auto-upgrade)
    (define-key map (kbd "U") 'borg-queen-mark-for-auto-upgrade-or-downgrade)
    (define-key map (kbd "c") 'borg-queen-mark-for-auto-upgrade-to-commit)
    (define-key map (kbd "C") 'borg-queen-mark-for-upgrade-to-commit)
    (define-key map (kbd "t") 'borg-queen-mark-for-auto-upgrade-to-tag)
    (define-key map (kbd "T") 'borg-queen-mark-for-upgrade-to-tag)

    (define-key map (kbd "A") 'borg-queen-mark-for-assimilation)
    (define-key map (kbd "R") 'borg-queen-mark-for-removal)

    (define-key map (kbd "-") 'borg-queen-unmark)

    (define-key map (kbd "x") 'borg-queen-run-marks)

    (define-key map (kbd "a") 'borg-queen-assimilate)
    (define-key map (kbd "d") 'borg-queen-describe)
    (define-key map (kbd "f") 'borg-queen-fork)

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
                                 ("Package" ,(-max (mapcar 'length (borg-clones))) t)
                                 ("Mark" 16 t)
                                 ("Type" 5 t)
                                 ("Upd" 6 t)
                                 ("Installed" 20 t)
                                 ("Commits" 7 t) ;; Commits
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
                   (props (cdr (assoc name borg-queen-drones-properties)))
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
                 ,(propertize name 'face (if (lax-plist-get props :required)
                                             'borg-queen-package-name-required-face
                                           'borg-queen-package-name-face))

                 ;; Mark
                 ,(if mark
                      (format
                       (alist-get (car mark) borg-queen-marks-reprs)
                       (cdr mark))
                    "")

                 ;; Type
                 ,(if (plist-get state :assimilated)
                      (propertize "Drone" 'face 'borg-queen-type-drone-face)
                    (propertize "Clone" 'face 'borg-queen-type-clone-face))

                 ;; Upgrade strategy
                 ,(let ((strategy (borg-queen-get-upgrade-strategy name)))
                    (propertize
                     (cond ((equal strategy 'tags) "Tag")
                           ((equal strategy 'commits) "Com")
                           (t " λ "))
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

(defun borg-queen-set-property (drone prop value)
  "Assign DRONE the property PROP with value VALUE.

For supported properties, see documentation for
`borg-queen-drone-properties'"

  (plist-put borg-queen-drones-properties drone
             (plist-put (plist-get drone borg-queen-drones-properties) prop value)))

(defun borg-queen-get-upgrade-strategy (drone)
  "Return the upgrade strategy for drone."
  (--if-let
      (borg-queen--aplist-get drone :upgrade-strategy borg-queen-drones-properties)
      it
    borg-queen-upgrade-strategy))

(defun borg-queen--state ()
  "Return the state of the Collective.

The returned value is an alist of plist.  The keys of the alist
are package names, as strings.  The keys of each plist are as
follows:

 `:name': drone name as a string.

 `:path': path to the git submodule.

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
                                     (borg-queen-git-verify drone "tag" version))
                                   (borg-queen-git-verify drone))))

                 `(,drone
                   ;; :path
                   (:path ,default-directory)

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

                   ;; numeric signature
                   (:signatures ,signatures)

                   ;; warnings
                   ,(when (and
                           (equal 'tags (borg-queen-get-upgrade-strategy drone))
                           (not tags))
                      `(:warnings "No tags in this repository, yet it is configured to update on tags only."))

                   ;; errors
                   ,(when (and (borg-queen-gpg-require-signature drone)
                               (null signatures))
                      `(:errors "Missing or invalid signature!"))
                   ))))
            (borg-clones))))

(defmacro borg-queen--with-selection (&rest body)
  "Execute BODY with each selected entry, binding PACKAGE to its name."
  `(save-excursion
     (if mark-active
         nil
       (let ((package (tabulated-list-get-id)))
         (progn ,@body)))))

(defun borg-queen--mark (drone &rest mark)
  "Add MARK to DRONE.

DRONE is a drone or clone name as a string.

MARK is nil, or a list whose car is a function name, and cdr its
arguments except the drone name.

If DRONE already has a mark, it is replaced."
  (if mark
    (map-put borg-queen--marks drone mark)
    (map-delete borg-queen--marks drone))
  (tabulated-list-print t))

(defun borg-queen-mark-for-auto-upgrade ()
  "@TODO"
  (interactive))
(defun borg-queen-mark-for-auto-upgrade-or-downgrade () "@TODO" (interactive))
(defun borg-queen-mark-for-auto-upgrade-to-commit () "@TODO" (interactive))
(defun borg-queen-mark-for-upgrade-to-commit () "@TODO" (interactive))
(defun borg-queen-mark-for-auto-upgrade-to-tag () "@TODO" (interactive))
(defun borg-queen-mark-for-upgrade-to-tag () "@TODO" (interactive))

(defun borg-queen-mark-for-assimilation ()
  "@TODO"
  (interactive)
  (borg-queen--with-selection
   (if (borg-queen--aplist-get package :assimilated borg-queen--state)
       (message "Already assimilated!")
     (borg-queen--mark package 'borg-queen--assimilate-action))))

(defun borg-queen-mark-for-removal ()
  "Mark DRONE for removal."
  (interactive)
  (borg-queen--with-selection
   (when (y-or-n-p (format "Mark %s for removal?" package))
     (borg-queen--mark package 'borg-queen--remove-action))))

(defun borg-queen-unmark ()
  "@TODO"
  (interactive)
  (borg-queen--with-selection
   (borg-queen--mark package)))

(defun borg-queen-run-marks ()
  "Execute marks."
  (interactive)
  (when (and borg-queen--marks
             (y-or-n-p (format "Execute %s mark(s)?" (length borg-queen--marks))))
    (dolist (mark borg-queen--marks)
      (eval `(,(cadr mark)
             ,(car mark)
             ,(cddr mark))))))

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
(defun borg-queen-commit () "@TODO" (interactive))
(defun borg-queen-fetch-and-refresh () "@TODO" (interactive))

(defun borg-queen--upgrade-action (version drone)
  "Set DRONE to VERSION.")

(defalias 'borg-queen--downgrade-action 'borg-queen-upgrade-action
  "Alias defined for UI use, to distinguish between upgrade and downgrade marks.")

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

(defun borg-queen-gpg-verify (drone &optional what object)
  "PGP-verify DRONE with \"git verify-WHAT OBJECT\".

WHAT can be nil, \"commit\" or \"tag\".  If nil, WHAT defaults to
commit.  If WHAT is \"tag\", OBJECT must be provided."
  (unless what
    (setq what "commit"))
  (unless object
    (if (equal "commit" what)
        (setq object "HEAD")
      (error "Can't guess a tag name")))

  (let* ((default-directory (expand-file-name drone borg-drone-directory))
         (signatures (borg-queen--gpg-parse-verify-output
                      (split-string
                       (shell-command-to-string (format "git verify-%s --raw %s" what (shell-quote-argument object)))
                       (rx "\n")))))
    ;; @FIXME Match signatures against list of valid signatures for DRONE.

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
             ;; The regexp below is based on the actual output of the program and documentation here:
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

(provide 'borg-queen)

;;; borg-queen.el ends here

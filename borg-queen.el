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
  "Manage and update Borg drones."
  )

(defgroup borg-queen-faces nil
  "Faces for Borg Queen."
  :group 'borg-queen
  )

(defcustom borg-queen-pgp-verify
  nil
  "If t, no package will be updated without a valid PGP signature."
  :group 'borg-queen
  )

(defcustom borg-queen-pgp-require-key-list
  nil
  "If t, no package signature will be verified unless a list if valid keys is provided."
  :group 'borg-queen
  )

(defcustom borg-queen-upgrade-strategy
  'commit
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
  hex key identifiers, either long or short form, without any
  suffix or prefix.
  Eg: (\"68CD0D65971CD850C96AA335D10A081695D25BA7\",
  \"33CDE511\") (case doesn't matter)

  This property has _LOWER_ priority than `:pgp-no-verify'.

  `:pgp-no-verify' -- t to locally override
  `borg-queen-pgp-always-verify'.  This has _HIGHER_ priority
  than `:pgp-verify'."
  :group 'borg-queen
  )

(defface borg-queen-package-name-face
  '((t :inherit italic))
  "Face for package names"
  :group 'borg-queen-faces
  )

(defface borg-queen-package-name-required-face
  '((t :inherit bold))
  "Face for required package names"
  :group 'borg-queen-faces
  )

(defface borg-queen-type-clone-face
  '((t :inherit bold))
  "Face for Type column when type is Clone."
  :group 'borg-queen-faces
  )

(defface borg-queen-type-drone-face
  '((t :inherit shadow))
  "Face for Type column when type is Drone."
  :group 'borg-queen-faces)

(defface borg-queen-upgrade-strategy-face
  '((t :inherit default))
  "Face for the upgrade strategy."
  :group 'borg-queen-faces)

(defface borg-queen-upgrade-strategy-error-face
  '((t :background "red" :foreground "white"))
  "Face for the upgrade strategy when the strategy is 'tag but the repository contains no tags."
  :group 'borg-queen-faces)

(defface borg-queen-upgradable-version-face
  '((t :foreground "#0088cc" :background "#AAAAAA"))
  "Face for Type column when type is Drone."
  :group 'borg-queen-faces
  )

(defvar borg-queen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") 'borg-queen-mark-for-auto-upgrade)
    (define-key map (kbd "U") 'borg-queen-mark-for-auto-upgrade-or-downgrade)
    (define-key map (kbd "c") 'borg-queen-mark-for-auto-upgrade-to-commit)
    (define-key map (kbd "C") 'borg-queen-mark-for-upgrade-to-commit)
    (define-key map (kbd "t") 'borg-queen-mark-for-auto-upgrade-to-tag)
    (define-key map (kbd "T") 'borg-queen-mark-for-upgrade-to-tag)

    (define-key map (kbd "a") 'borg-queen-mark-for-assimilation)
    (define-key map (kbd "a") 'borg-queen-mark-for-assimilation)
    map)
  "Keymap for Borg Queen mode.")

(defvar-local borg-queen--state
  nil
  "The state of the Collective, for use by its Queen.

For a more formal documentation of this variable, see
documentation for function `borg-queen--state'.")

(define-derived-mode borg-queen-mode tabulated-list-mode "Borg Queen"
  "Major mode for the Borg Queen."
  :group 'borg-queen
  (setq tabulated-list-format `[
                                ("Package" ,(-max (mapcar 'length (borg-clones))) t)
                                ("Type" 5 t)
                                ("Upd" 6 t)
                                ("Installed" 20 t)
                                ("Commits" 6 t)
                                ("Available" 20 t)
                                ("Operation" 99 t)
                                ]
        tabulated-list-sort-key (cons "Package" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun borg-queen ()
  "Manage and update Borg drones."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Borg Queen*"))
  (borg-queen-mode)
  (setq borg-queen--state (borg-queen--state))
  (setq tabulated-list-entries 'borg-queen--tabulated-list-entries)
  (tabulated-list-print)
  )

(defun borg-queen--tabulated-list-entries ()
  "Build the list of tabulated list entries."
  (mapcar (lambda (item)
            (let* ((name (car item))
                  (state (cdr item))
                  (props (cdr (assoc name borg-queen-drones-properties)))
                  )
            `(,name
              [
               ;; Name
               ,(propertize name 'face (if (lax-plist-get props :required)
                                           'borg-queen-package-name-required-face
                                           'borg-queen-package-name-face))

               ;; Type
               ,(if (lax-plist-get state :assimilated)
                   (propertize "Drone" 'face 'borg-queen-type-drone-face)
                  (propertize "Clone" 'face 'borg-queen-type-clone-face))

               ;; Update strategy
               ,(--if-let (lax-plist-get props :upgrade-strategy)
                    (cond ((equal it :tags) (propertize "T" 'face (if (lax-plist-get state :latest-tag)
                                                                      'default
                                                                      'borg-queen-upgrade-strategy-error-face)))
                         ((equal it :commits) "C")
                         (t "λ"))
                  "")

               ;; Installed version
               ,(lax-plist-get state :version)

               ;; New commits
               ,(let ((count (lax-plist-get state :new-commits)))
                  (propertize (number-to-string count)
                              'face (if (> count 0) 'default 'shadow)))

               ;; Available
               ,(-if-let* ((latest-tag (lax-plist-get state :latest-tag))
                           (_ (not (equal latest-tag (lax-plist-get state :version-last-tag)))))
                    latest-tag
                  "")

               ""
              ]
            )))
          borg-queen--state))

(defun borg-queen-set-property (drone prop value)
  "Assign DRONE the property PROP with value VALUE.

For supported properties, see documentation for
`borg-queen-drone-properties'"

  (plist-put borg-queen-drones-properties drone
             (plist-put (plist-get drone borg-queen-drones-properties) prop value)))
;;
(defun borg-queen-get-property (drone prop &optional no-default)
  "Return property PROP if it has been set for DRONE, or the value of the corresponding Customize variable."
  (let ((properties (lax-plist-get borg-queen-drones-properties drone)))

    ;; :pgp-verify is a special case because it has a negative
    ;; override.  If :pgp-no-verify is t, we return nil without
    ;; further checks.
    (if (and (eq prop :pgp-verify)
             (plist-get properties :pgp-no-verify))
        nil
                                        ; Default case
      (or
       (plist-get properties prop)
       (unless no-default
         (cond
        ;; If we're still here, prop is nil so we have to fallback to
        ;; the Customize variable.
          ((eq prop :pgp-verify) borg-queen-pgp-verify)
          ((eq prop :upgrade-strategy) borg-queen-upgrade-strategy)))))))

(defun borg-queen--state (&optional previous-state)
  "Return the state of the Collective, merged with PREVIOUS-STATE.

PREVIOUS-STATE must be a value previously returned by this
function.  If provided, the value of user-provided
keys (eg :mark) is copied from this state.

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

 `:version-last-tag': if `version-type' is 0, then this is equal to
 `version'.  If `version-type' is 1, it contains the name of the
 most recent tag this commit is based on.  Otherwise, its value
 is nil.

 `:assimilated': t if DRONE is registered as a submodule.

 `:new-commits': the number of new commits.

 `:latest-tag': the most recent tag.

 `:action': the action the user wants to perform on this drone.
 Set only if it is non-nil in PREVIOUS-STATE."
  (let ((borg-drones (borg-drones)))
    (mapcar (lambda (drone)
              (-flatten
               (let ((default-directory (expand-file-name drone borg-drone-directory)))
                 `(,drone
                   ;; :path
                   (:path ,default-directory)

                   ;; :version, :version-type, :version-last-tag
                   ,(or
                     (--when-let (magit-git-lines "describe" "--tags" "--exact-match")
                       `(:version-type 0 :version ,(car it) :version-last-tag ,(car it)))
                     (--when-let (magit-git-lines "describe" "--tags")
                       `(:version-type 1 :version ,(car it) :version-last-tag ,(magit-git-lines "describe" "--tags" "--abbrev=0")))
                     (--when-let (magit-git-lines "describe" "--tags" "--always")
                       `(:version-type 2 :version ,(car it))))

                   ;; :latest-tag
                    ,(--when-let (magit-git-lines "tag"  "--sort" "creatordate"  "--merged" "origin/master")
                       ;; @FIXME Don't assume origin branch name.                                   ^^^^^^
                       `(:latest-tag ,(-last-item it)))

                    ;; :assimilated
                    ,(when (member drone borg-drones)
                       '(:assimilated t))

                    ;; :new-commits
                    (:new-commits ,(length (magit-git-lines "log" "--format=oneline" "HEAD..origin")))))))
            (borg-clones))))

(defun borg-queen--aplist-get (pkey akey aplist)
  "Return the value associated to PKEY in a plist associated to AKEY in APLIST."
  (lax-plist-get (cdr (assoc akey aplist)) pkey))

(provide 'borg-queen)

;;; borg-queen.el ends here

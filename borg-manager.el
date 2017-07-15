;;; borg-manager -- Manage, maintain and upgrade Borg packages.

;; Copyright (c) 2017 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Homepage: https://github.com/thblt/borg-manager
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
(require 'magit)
(require 'tabulated-list)

(defgroup borg-manager nil
  "Manage and update Borg drones."
  )

(defgroup borg-manager-faces nil
  "Faces for Borg Manager."
  :group 'borg-manager
  )

(defcustom borg-manager-pgp-verify
  nil
  "If t, no package will be updated without a valid PGP signature."
  :group 'borg-manager
  )

(defcustom borg-manager-pgp-require-key-list
  nil
  "If t, no package signature will be verified unless a list if valid keys is provided."
  :group 'borg-manager
  )

(defcustom borg-manager-upgrade-strategy
  'commit
  "Determine whether to upgrade to tags or commits.

If 'tag, packages will be auto upgraded to the tag describing the
highest version number.  If 'commit or any other value, auto
upgrade will go the most recent commit.

'tag (roughly) mimics the behavior of using Melpa Stable.

This property can be overriden for specific drones with `borg-manager-drone-properties'"
  :group 'borg-manager)

(defcustom borg-manager-drones-properties
  '("borg" (:required t))
  "An alist of plists of drones properties for use with Borg Manager.

Entries are of them form (DRONE PROPERTIES), where PROPERTIES is
  either nil or a plist with the following entries:

  `:required' -- t if the package is explicitely required by your
  configuration.  This will be used by the upcoming dependency
  tracker to identify orphan drones.  (Default: nil)

  `:upgrade-remote' -- the name of the remote to upgrade from.
  (Default: upstream if set, or the first remote, if not.)

  `:upgrade-strategy' -- either 'tag or 'commit (Default:
  `borg-manager-upgrade-strategy`, see this variable for details)

  `:pgp-verify' -- Either t to accept valid signatures from any
  key present in gpg keyring, or a key identifier or a list of
  hex key identifiers, either long or short form, without any
  suffix or prefix.
  Eg: (\"68CD0D65971CD850C96AA335D10A081695D25BA7\",
  \"33CDE511\") (case doesn't matter)

  This property has _LOWER_ priority than `:pgp-no-verify'.

  `:pgp-no-verify' -- t to locally override
  `borg-manager-pgp-always-verify'.  This has _HIGHER_ priority
  than `:pgp-verify'."
  :group 'borg-manager
  )

(defface borg-manager-package-name-face
  '((t :inherit italic))
  "Face for package names"
  :group 'borg-manager-faces
  )

(defface borg-manager-package-name-required-face
  '((t :inherit bold))
  "Face for required package names"
  :group 'borg-manager-faces
  )

(defface borg-manager-type-clone-face
  '((t :inherit bold))
  "Face for Type column when type is Clone."
  :group 'borg-manager-faces
  )

(defface borg-manager-type-drone-face
  '((t :inherit shadow))
  "Face for Type column when type is Drone."
  :group 'borg-manager-faces
  )

(defface borg-manager-upgradable-version-face
  '((t :foreground "#0088cc" :background "#AAAAAA"))
  "Face for Type column when type is Drone."
  :group 'borg-manager-faces
  )

(defvar borg-manager-mode-map
  (let ((map (make-keymap)))
    (define-key map "u" 'borg-manager-mark-for-upgrade)
    map)
  "Keymap for Borg Manager mode.")

(define-derived-mode borg-manager-mode tabulated-list-mode "BorgMngr"
  "Major mode for the Borg Manager."
  :group 'borg-manager
  (setq tabulated-list-format `[
                                ("Package" ,(-max (mapcar 'length (borg-clones))) t)
                                ("Type" 5 t)
                                ("Upd" 6 t)
                                ("Installed" 20 t)
                                ("Available" 20 t)
                                ("Operation" 99 t)
                                ]
        tabulated-list-sort-key (cons "Package" nil))
  (tabulated-list-init-header)
  (use-local-map borg-manager-mode-map))

;;;###autoload
(defun borg-manager ()
  "Manage and update Borg drones."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Borg Manager*"))
  (borg-manager-mode)
  (setq tabulated-list-entries (borg-manager--drones-states-list))
  (tabulated-list-print)
  )

(defun borg-manager-set-property (drone prop value)
  "Assign DRONE the property PROP with value VALUE.

For supported properties, see documentation for
`borg-manager-drone-properties'"

  (plist-put borg-manager-drones-properties drone
             (plist-put (plist-get drone borg-manager-drones-properties) prop value)))
;;
(defun borg-manager-get-property (drone prop &optional no-default)
  "Return property PROP if it has been set for DRONE, or the value of the corresponding Customize variable."
  (let ((properties (lax-plist-get borg-manager-drones-properties drone)))

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
          ((eq prop :pgp-verify) borg-manager-pgp-verify)
          ((eq prop :upgrade-strategy) borg-manager-upgrade-strategy)))))))

(defun borg-manager--drones-states-list ()
  "@TODO Write documentation"
  (mapcar (lambda (drone)
            `(,drone
              ,(let ((props (borg-manager--drone-state drone)))
                 `[
                   ;; Name
                   ,(propertize drone 'face (if (borg-manager-get-property drone :required)
                                                'borg-manager-package-name-required-face
                                                'borg-manager-package-name-face))

                   ;; Type
                   ,(if (plist-get props :assimilated)
                        #("Drone" 0 5 (face borg-manager-type-drone-face))
                      #("Clone" 0 5 (face borg-manager-type-clone-face)))

                   ;; Update strategy (Upd)
                   ,(--if-let (borg-manager-get-property drone :upgrade-strategy t)
                        (cond
                         ((equal it 'tag) "T")
                         ((equal it 'commit) "C")
                         (t "λ")
                          )
                      ""
                      )

                   ;; Installed
                   ,(plist-get props :version)

                   ;; Available
                   ,(--if-let (plist-get props :latest-tag)
                        (propertize it 'face (if (equal it (plist-get props :version-last-tag)) 'shadow 'borg-manager-upgradable-version-face))
                      ""
                      )
                   ;; Operation
                   ""
                   ])))
          (borg-clones)))

(defun borg-manager--drone-state (drone)
  "Return a plist describing the state of DRONE.

Returned keys are:
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

 `assimilated': t if DRONE is registered as a submodule.

 `new-commits': the number of new commits.

 `latest-tag': the most recent tag."
  (let ((default-directory (expand-file-name drone borg-drone-directory)))
    (-flatten
     `(
       ;; name
       (:name ,drone)

       ;; :path
       (:path ,default-directory)

       ;; :version, :version-type, :version-last-tag
     ,(or
       (--when-let (magit-git-lines "describe" "--exact-match") `(:version-type 0 :version ,(car it) :version-last-tag ,(car it)))
       (--when-let (magit-git-lines "describe") `(:version-type 1 :version ,(car it) :version-last-tag ,(magit-git-lines "describe" "--abbrev=0")))
       (--when-let (magit-git-lines "describe" "--always")  `(:version-type 2 :version ,(car it))))

     ;; :last-tag
     ,(--when-let (magit-git-lines "tag")
        `(:latest-tag ,(-last-item it)))

     ;; :assimilated
     ,(when (member drone (borg-drones))
        '(:assimilated t))))))

(provide 'borg-manager)

;;; borg-manager.el ends here

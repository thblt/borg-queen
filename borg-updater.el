;;; borg-manager -- manage and update Borg drones

;;; Commentary:

;;; Code:

(require 'borg)
(require 'magit)
(require 'tabulated-list)

(defgroup borg-manager nil
  "Manage and update Borg drones."
  )

(defcustom borg-manager-pgp-require-verification
  nil
  "If t, no package will be updated without a valid PGP signature."
  )

(defcustom borg-manager-pgp-require-key-list
  nil
  "If t, no package signature will be verified unless a list if valid keys is provided."
  )

(defcustom borg-manager-upgrade-tags-only
  nil
  "If t, drones will be upgraded to tagged commits only.

This (roughly) mimics the behavior of using Melpa Stable.

This only determines which packages are considered upgradable and
the behavior of borg-manager-mark-for-upgrade.

This property can be overriden for specific drones with `borg-manager-drone-properties'"
  :group 'borg-manager)

(defcustom borg-manager-drones-properties
  '()
  "A list of drones properties for use with Borg Manager.

Entries are of them form (DRONE PROPERTIES), where PROPERTIES is
  either nil or a plist with the following entries:

  `:required' -- t if the package is explicitely required by your
  configuration.  This will be used by the upcoming dependency
  tracker to identify orphan drones.  (Default: nil)

  `:upgrade-remote' -- the name of the remote to upgrade from.
  (Default: upstream if set, or the first remote, if not.)

  `:track-tags-only' -- when t, indicates that this package should be
  updated only whenever a new tag is available, instead of at
  each commit.  (Default: `borg-manager-track-tags-only`)

  `:pgp-verify' -- Either nil for no verification to take place,
  t to accept any valid signatures from a key present in gpg
  keyring or either a key identifier or a list of key
  identifiers."
  :group 'borg-manager
  )

(defface borg-manager-package-name-face
  '((t :inherit default))
  "Face for package names"
  )

(defface borg-manager-type-clone-face
  '((t :inherit bold))
  "Face for type column when type is Clone."
  )

(defface borg-manager-type-drone-face
  '((t :inherit shadow))
  "Face for type column when type is Drone."
  )

(defvar borg-manager-mode-map
  (let ((map (make-keymap)))
    (define-key map "u" 'borg-manager-mark-for-upgrade)
    map)
  "Keymap for Borg Manager mode.")

(define-derived-mode borg-manager-mode tabulated-list-mode "BorgMngr"
  "Major mode for the Borg Manager."
  (setq tabulated-list-format `[
                                ("Package" ,(-max (mapcar 'length (borg-clones))) t)
                                ("Type" 5 t)
                                ("Installed" 20 t)
                                ("Available" 20 t)
                                ]
        tabulated-list-sort-key (cons "Package" nil))
  (tabulated-list-init-header)
  (use-local-map borg-manager-mode-map))

(defun borg-manager ()
  "Manage and update Borg drones."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Borg Manager*"))
  (borg-manager-mode)
    (setq tabulated-list-entries (borg-manager--build-list))
  (tabulated-list-print)
  )

(defun borg-manager-mark-for-upgrade ()
  (tablist-mark-forward)

  )

(defun borg-manager--build-list ()
  "@TODO Write documentation"
        (mapcar (lambda (drone)
                  `(,drone
                    ,(let ((props (borg-manager--drone-properties drone)))
                       `[
                         ,drone
                         ,(if (plist-get props :assimilated)
                              #("Drone" 0 5 (face borg-manager-type-drone-face))
                            #("Clone" 0 5 (face borg-manager-type-clone-face)))
                         ,(plist-get props :version)
                         #("None" 0 4 (face shadow))
                         ])))
                (borg-clones)))

(defun borg-manager--drone-properties (drone)
  "Return an alist of properties for DRONE.

Returned properties are:

 `:path': path to the git submodule.

 `:version': either the output of `git describe' or the short hash
 of the current commit (if `version-type' == 2).

 `:version-type': 0 if the current commit is exactly a tag, 1 if
 it's a tag + a number of changes, 2 if it's a commit without a
 previous tag.

 `:version-tag': if `version-type' is 0, then this is equal to
 `version'.  If `version-type' is 1, it contains the name of the
 most recent tag this commit is based on.  Otherwise, its value
 is nil.

 `assimilated': t if DRONE is registered as a submodule.

 `new-commits': the number of new commits.

 `last-tag': the most recent tag."
  (let ((default-directory (expand-file-name drone borg-drone-directory)))
    (-flatten
     `(:path ,default-directory

             ,(or
               (--when-let (magit-git-lines "describe" "--exact-match") `(:version-type 0 :version ,(car it)))
               (--when-let (magit-git-lines "describe") `(:version-type 1 :version ,(car it)))
               (--when-let (magit-git-lines "log" "-n" "1" "--format=%h")  `(:version-type 2 :version ,(car it))))

             ,(when (member drone (borg-drones))
                '(:assimilated t))
             ))))

(borg-manager--drone-properties "eziam-theme-emacs")
(borg-manager--drone-properties "helm")
(borg-manager--drone-properties "../etc/yasnippet/snippets")
(borg-manager--drone-properties "../etc/")

;;; borg-manager.el ends here

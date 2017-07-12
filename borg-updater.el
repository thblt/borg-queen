;;; borg-updater -- manage and update Borg drones

;;; Commentary:

;;; Code:

(require 'borg)
(require 'magit)
(require 'tabulated-list)

(defgroup borg-updater nil
  "Manage and update Borg drones."
  )

(defcustom borg-updater-require-pgp-verification
  nil
  "Soit t, et une clé valide est requise pour chaque paquet."
  )

(defcustom borg-updater-require-explicitly-valid-key
  nil
  "Require that each package define a list of PGP keys allowed to sign this package."
  )

(defcustom borg-updater-track-tags-only
  nil
  "If t, a drone will be considered upgradable if only if there's a tagged commit available.

This (roughly) mimics the behavior of using Melpa Stable.

This property can be overriden for specific drones with `borg-updater-drone-properties'"
  :group 'borg-updater)

(defcustom borg-updater-drones-properties
  '()
  "A list of drones properties for use with Borg Updater.

Entries are of them form (DRONE PROPERTIES), where PROPERTIES is
  either nil or a plist with the following entries:

  `:required' -- t if the package is explicitely required by your
  configuration.  This will be used by the upcoming dependency
  tracker to identify orphan drones.  (Default: nil)

  `:upgrade-remote' -- the name of the remote to upgrade from.
  (Default: upstream if set, or the first remote, if not.)

  `:track-tags-only' -- when t, indicates that this package should be
  updated only whenever a new tag is available, instead of at
  each commit.  (Default: `borg-updater-track-tags-only`)

  `:pgp-verify' -- Either nil for no verification to take place,
  t to accept any valid signatures from a key present in gpg
  keyring or either a key identifier or a list of key
  identifiers."
  :group 'borg-updater
  )

(defvar borg-updater-mode-map
  (let ((map (make-keymap)))
    (define-key map "u" 'borg-updater-mark-for-upgrade)
    map)
  "Keymap for Borg Updater mode.")

(define-derived-mode borg-updater-mode tabulated-list-mode "BorgUpd"
  "Major mode for the Borg updater."
  (setq tabulated-list-format `[
                                ("Package" ,(-max (mapcar 'length (borg-clones))) t)
                                ("Type" 5 t)
                                ("Installed" 20 t)
                                ("Available" 20 t)
                                ]
        tabulated-list-sort-key (cons "Package" nil))
  (tabulated-list-init-header)
  (use-local-map borg-updater-mode-map))

(defun borg-updater ()
  "Manage and update Borg drones."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Borg Updater*"))
  (borg-updater-mode)
    (setq tabulated-list-entries (borg-updater--build-list))
  (tabulated-list-print)
  )

(defun borg-updater-mark-for-upgrade ()
  (tablist-mark-forward)

  )

(defun borg-updater--build-list ()
  "@TODO Write documentation"
        (mapcar (lambda (drone)
                  `(,drone
                    ,(let ((props (borg-updater--drone-properties drone)))
                       `[
                         ,drone
                         ,(if (plist-get props :assimilated)
                              #("Drone" 0 5 (face shadow))
                            #("Clone" 0 5 (face bold)))
                         ,(plist-get props :version)
                         #("None" 0 4 (face shadow))
                         ])))
                (borg-clones)))

(borg-updater--build-list)

(defun borg-updater--drone-properties (drone)
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

(borg-updater--drone-properties "eziam-theme-emacs")
(borg-updater--drone-properties "helm")
(borg-updater--drone-properties "../etc/yasnippet/snippets")
(borg-updater--drone-properties "../etc/")

;;; borg-updater.el ends here

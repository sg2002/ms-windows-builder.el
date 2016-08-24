;;; emacs-win-builder.el --- Elisp script for quckly building emacs on Windows.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 sg2002 <sg2002@gmx.com>

;; Author: sg2002 <sg2002@gmx.com>
;; Keywords: internal, windows

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar ewb-program-folders '("c:/Program Files/" "c:/Program Files (x86))/"))

(defun ewb-lame-search (paths subdir)
  "Temproary, while I don't understand why locate-file is not working here."
  (cl-some (lambda (path)
             (if (file-exists-p (concat path subdir))
                 (concat path subdir)
               nil))
           paths))

(defun ewb-ensure-7zip ()
  "Ensure we have 7zip on our path for unzipping files."
  (when (not (executable-find "7z"))
    (file-exists-p )))

(defun ewb-ensure-wget ()
  "Ensure we have wget on our path for downloading dependencies.  Wget works nicely, since it's able to get stuff from sourceforge."
  (if (executable-find "wget")
      t
    nil))

(defun ewb-ensure-mingw ()
  "Ensure we have mingw installed."
  )

(defun ewb-ensure-mingw-build-extras ()
  "Ensure we have all the extras needed for 32 bit MinGW build installed.")

(defun ewb-ensure-git ()
  "Ensure we have git installed.")

(defun ewb-ensure-emacs-source ()
  "Ensure our source is checked out.")

(defun ewb-build-mingw ()
  "Build emacs using 32 bit MinGW."
  (ewb-configure)
  (ewb-make)
  (ewb-make-install))

(defun ewb-build-mingw64-msys2 ()
  "Build emacs using 64 bit MinGW."
  (ewb-build-full ewb-msys2-path '("MSYSTEM=MINGW64")
                  "d:/Projects/Emacs/building/mingw64-Og/" "/c/Emacs/25-dev-mingw64/")

  (defvar ewb-msys2-path
    (concat "/mingw64/bin:/usr/local/bin:/usr/bin:"
            "/bin:/c/Windows/System32:/c/Windows:"
            "/c/Windows/System32/Wbem:/c/Windows/System32/WindowsPowerShell/v1.0/:"
            "/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl")))

(defvar ewb-emacs-source "d:/Projects/Emacs/building/repo/")

(defun ewb-build-full (path extra-env configuration-dir destination-dir)
  "Build emacs using 64 bit MinGW."
  (ewb-autogen path extra-env)
  (ewb-configure path extra-env configuration-dir destination-dir)
  (ewb-make path extra-env configuration-dir)
  (ewb-make-install path extra-env configuration-dir))

(defun ewb-autogen (path extra-env)
  (ewb-command path extra-env "./autogen.sh" ewb-emacs-source))

(defun ewb-configure (path extra-env configuration-dir prefix)
  (ewb-command path (append extra-env ewb-configure-env)
               (concat "eval " ewb-emacs-source "/configure" " \"" ewb-configure-args " --prefix=" prefix "\"")
               configuration-dir))

(defvar ewb-configure-env '("PKG_CONFIG_PATH=/mingw64/lib/pkgconfig"
                            "CFLAGS=-Og -gdwarf-4 -g3"))

(defvar ewb-configure-args "--without-imagemagick --enable-checking='yes,glyphs' --enable-check-lisp-object-type")

(defun ewb-make (path extra-env configuration-dir)
  (ewb-command path extra-env
               (concat "make -j " (number-to-string ewb-make-threads))
               configuration-dir))

(defvar ewb-make-threads 1)

(defun ewb-make-install (path extra-env configuration-dir)
  (ewb-command path extra-env
               "make install"
               configuration-dir))

(defun ewb-command (path extra-env command dir)
  (let* ((shell-file-name "bash")
         ;; By using lexical binding we can use setenv and getenv
         ;; on our local version of process-environment.
         (process-environment (append process-environment extra-env))
         (process-environment (progn (setenv "PATH" path)
                                     ;; Default LANG may screw up automake
                                     ;; version detection in autogen.sh.
                                     (setenv "LANG" "")
                                     process-environment)))
    (cd dir)
    (process-file-shell-command command nil "ewb")))

(provide 'emacs-win-builder)
;;; emacs-win-builder.el ends here

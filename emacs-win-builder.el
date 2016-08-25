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

;; This script installs the required tools and then builds Emacs from the
;; source code.

;; The only requirement is wget.  You can get it from GnuWin project:
;; http://gnuwin32.sourceforge.net/packages/wget.htm
;; Then either put it on PATH or put the path to it into ewb-wget-paths.
;; Path for GnuWin version is already set up.
;; Anything downloaded by this script using wget gets stored to
;; ewb-wget-download-path and reused if it's already there.

;; 7Zip would get installed, unless it's already present on your PATH.

;; Full build starting from the toolchain setup would take at least 30 minutes.

;; Msys2 specific:
;; For msys2 make sure that c:\msys64 directory is writable.
;; During msys2 setup you would get a shell window.  You can close
;; it after it's done with setup and you see the command prompt.

;; Knwon issues:
;; This script is completely synchronous and would freeze your Emacs session.
;; Running it in a separate session is recommended.
;; If you have a toolchain(MinGW or msys2) already installed, but it does not
;; have all the required components, this script would break.  You can manually
;; install components into existing toolchain using (ewb-msys2-install-packages)
;; for msys2 and for MinGW.

;;; Code:

;; * Main entrance points:
(defvar ewb-emacs-source "d:/Projects/Emacs/building/repo/")

(defun ewb-build-mingw64-msys2 ()
  "Build emacs using 64 bit MinGW."
  (ewb-msys2-ensure)
  (ewb-build-full ewb-msys2-exec-path ewb-msys2-path '("MSYSTEM=MINGW64")
                  "d:/Projects/Emacs/building/mingw64-Og/" "/c/Emacs/25-dev-mingw64/"))

;; * Wget

(defun ewb-wget-download-file (file)
  (let* ((file-tokens (reverse (split-string file "/")))
         ;; Sourceforge filenames have "download" at the end.
         (file-name (if (eq "download" (car file-tokens))
                        (car file-tokens)
                        (cadr file-tokens)))
         (local-file (concat ewb-wget-download-path file-name)))
    (when (not (file-exists-p local-file))
      (when (not (file-exists-p ewb-wget-download-path))
        (mkdir ewb-wget-download-path))
      (cd ewb-wget-download-path)
      (process-file-shell-command (concat "\"" (ewb-get-wget) "\" --no-check-certificate " file) nil "ewb"))
    local-file))

(ewb-wget-download-file ewb-msys2-x64-setup)

(defun ewb-get-wget ()
  "Ensure we have wget on our path for downloading dependencies.
 Wget works nicely, since it's able to get stuff from sourceforge."
 (or (executable-find "wget.exe")
     (locate-file "wget.exe" ewb-wget-paths)
     (error "Wget not found.")))

(defvar ewb-wget-paths '("c:/Program Files (x86)/GnuWin32/bin/" "c:/Program Files/GnuWin32/bin/"))

(defvar ewb-wget-download-path "c:/Emacs/downloads/")

;; * 7zip

(defun ewb-7z-extract (file destination)
  (process-file-shell-command
   (concat (ewb-get-7z) " x " file " -aoa -o"
           (replace-regexp-in-string "/" "\\\\" destination)) nil "ewb"))

(defun ewb-get-7z ()
  "Ensure we have 7zip on our path for unarchiving."
  (or (executable-find "7z.exe")
      (locate-file "7z.exe" ewb-7z-paths)
      (ewb-install-7z)))

(defun ewb-install-7z ()
  (let (setup-file (wget-download-file (if (ewb-windows-is-64-bit)
                                           ewb-7z-x64-setup
                                         ewb-7z-x32-setup))))
  (process-file-shell-command
   (concat setup-file " /S") nil "ewb")
  (locate-file "7z.exe" ewb-7z-paths))

(defvar ewb-7z-paths '("c:/Program Files/7-Zip/" "c:/Program Files (x86)/7-Zip/"))

(defvar ewb-7z-setup "http://repo.msys2.org/distrib/i686/msys2-i686-20160205.exe")

;; * MinGW

(defvar ewb-program-folders '("c:/Program Files/" "c:/Program Files (x86))/"))

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

;; * Msys2

(defvar ewb-msys2-exec-path '("C:/msys64/usr/bin/"))

(defvar ewb-msys2-path
  (concat "/mingw64/bin:/usr/local/bin:/usr/bin:"
          "/bin:/c/Windows/System32:/c/Windows:"
          "/c/Windows/System32/Wbem:/c/Windows/System32/WindowsPowerShell/v1.0/:"
          "/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/"))

(defvar ewb-msys2-extra-env '("MSYSTEM=MINGW64"))

(defun ewb-msys2-ensure ()
  ;; Need a much better check here...
  (when (not (file-exists-p "c:/msys64/"))
    (ewb-msys2-install)
    (ewb-msys2-install-packages)))

(defun ewb-msys2-install ()
  (let* ((dist (ewb-wget-download-file (if (ewb-windows-is-64-bit)
                                      ewb-msys2-x64-dist
                                      ewb-msys2-x32-dist)))
         (dist2 (substring dist 0 -3)))
    (ewb-7z-extract dist ewb-wget-download-path)
    (ewb-7z-extract dist2 "c:/"))
  ;; We need to initialize msys somehow, this ugly but there seems to be
  ;; no other way. Also launching msys2.exe does not do this, when
  ;; launched from emacs.
  (start-process-shell-command "msys2" "ewb" "c:/msys64/msys2_shell.cmd")
  (sleep-for 30))

(defvar ewb-msys2-x32-dist
  "https://sourceforge.net/projects/msys2/files/Base/i686/msys2-base-i686-20160719.tar.xz/download")

(defvar ewb-msys2-x64-dist
  "https://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-base-x86_64-20160719.tar.xz/download")

(defun ewb-msys2-install-packages ()
  (dolist (package ewb-msys2-packages)
    (ewb-msys2-install-package package)))

(defun ewb-msys2-install-package (package)
  (ewb-command ewb-msys2-exec-path ewb-msys2-path ewb-msys2-extra-env
               (concat "pacman -S --noconfirm --needed" package)))

(defvar  ewb-msys2-packages '("base-devel" "mingw-w64-x86_64-toolchain"
                             "mingw-w64-x86_64-xpm-nox" "mingw-w64-x86_64-libtiff"
                             "mingw-w64-x86_64-giflib" "mingw-w64-x86_64-libpng"
                             "mingw-w64-x86_64-libjpeg-turbo" "mingw-w64-x86_64-librsvg"
                             "mingw-w64-x86_64-libxml2" "mingw-w64-x86_64-gnutls"))

(ewb-msys2-install-packages)

;; * Generic functionality

(defun ewb-windows-is-64-bit ()
  (file-exists-p "c:/Program Files (x86)/"))

(defun ewb-build-full (exec-path path extra-env configuration-dir destination-dir)
  "Build Emacs using 64 bit MinGW."
  (ewb-autogen exec-path path extra-env)
  (ewb-configure exec-path path extra-env configuration-dir destination-dir)
  (ewb-make exec-path path extra-env configuration-dir)
  (ewb-make-install exec-path path extra-env configuration-dir))

(defun ewb-autogen (exec-path path extra-env)
  (ewb-command exec-path path extra-env "./autogen.sh" ewb-emacs-source))

(defun ewb-configure (exec-path path extra-env configuration-dir prefix)
  (ewb-command exec-path path (append extra-env ewb-configure-env)
               (concat "eval " ewb-emacs-source "/configure" " \"" ewb-configure-args " --prefix=" prefix "\"")
               configuration-dir))

(defvar ewb-configure-env '("PKG_CONFIG_PATH=/mingw64/lib/pkgconfig"
                            "CFLAGS=-Og -gdwarf-4 -g3"))

(defvar ewb-configure-args "--without-imagemagick --enable-checking='yes,glyphs' --enable-check-lisp-object-type")

(defun ewb-make (exec-path path extra-env configuration-dir)
  (ewb-command exec-path path extra-env
               (concat "make -j " (number-to-string ewb-make-threads))
               configuration-dir))

(defvar ewb-make-threads 1)

(defun ewb-make-install (exec-path path extra-env configuration-dir)
  (ewb-command exec-path path extra-env
               "make install"
               configuration-dir))

(defun ewb-command (local-exec-path path extra-env command &optional dir)
  (let* ((shell-file-name "bash")
         ;; By using lexical binding we can use setenv and getenv
         ;; on our local version of process-environment.
         (process-environment (append process-environment extra-env))
         (process-environment (progn (setenv "PATH" path)
                                     ;; Default LANG may screw up automake
                                     ;; version detection in autogen.sh.
                                     (setenv "LANG" "")
                                     process-environment))
         (exec-path local-exec-path))
    (when dir
      (cd dir))
    (process-file-shell-command command nil "ewb")))

(provide 'emacs-win-builder)
;;; emacs-win-builder.el ends here

;;; windows-builder.el --- Elisp script for quckly building emacs on Windows.  -*- lexical-binding: t; -*-

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

;; Requirements:
;; The only requirement is wget.  You can get it from GnuWin project:
;; http://gnuwin32.sourceforge.net/packages/wget.htm
;; Or from ezwinports:
;; https://sourceforge.net/projects/ezwinports/files/wget-1.16.1-w32-bin.zip/download
;; Then either put it on PATH or put the path to it into wb-wget-paths.
;; Path for GnuWin version is already set up.
;; Anything downloaded by this script using wget gets stored to
;; wb-wget-download-directory and reused if it's already there.

;; 7Zip would get installed, unless it's already present on your PATH.

;; Usage:
;; Make sure that depending on the build  wb-mingw-directory,
;; wb-msys2-x64-directory or wb-msys2-x32-directory is writable
;; and wb-emacs-source points to Emacs source repository.
;; Put the script on your load path and do:
;; (require 'windows-builder)
;; Alternatively just do eval-buffer on it.
;; Then use the build function.  For MinGW:
;; (wb-build 'mingw "c:/Emacs/builds/mingw" "c:/Emacs/25-dev-mingw")
;; For Msys2-x32:
;; (wb-build 'msys2-x32  "c:/Emacs/builds/msys2-x32" "c:/Emacs/25-dev-msys2-x32")
;; For Msys2-x64:
;; (wb-build 'msys2-x64  "c:/Emacs/builds/msys2-x64" "c:/Emacs/25-dev-msys2-x64")
;; Full build starting from the toolchain setup would take
;; at least 20 minutes for MinGW and 30 minutes for Msys2.

;; Msys2 specific:
;; During msys2 setup you would get a shell window.  You can close
;; it after it's done with setup and you see the command prompt.

;; Known issues:
;; This script is completely synchronous and would freeze your Emacs session.
;; Running it in a separate session is recommended.
;; If you have MinGW already installed and try to use the same location, but it does not
;; have all of the required components, this script would break.  You can manually
;; install components into existing toolchain using (wb-mingw-install-packages).
;; For Msys it should install all packages, you can also do it manually using
;; (wb-msys2-install-packages).

;; Troubleshooting:
;; All output gets saved to "wb" buffer.

;;; Code:

;; * Main
(defcustom wb-emacs-source "c:/Emacs/source"
  "*Directory that contains Emacs source code."
  :group 'wb
  :type 'directory)

(defun wb-build (selected-build make-path output-path)
  "Build Emacs using SELECTED-BUILD, which should be defined in wb-builds. Run
configure and make in MAKE-PATH. Install Emacs into OUTPUT-PATH."
  (let ((build (cadr (assoc selected-build wb-builds))))
    (funcall (cadr (assoc 'ensure-fn build)))
    (wb-build-full (funcall (cadr (assoc 'get-exec-path-fn build)))
                    (funcall (cadr (assoc 'get-path-fn build)))
                    (funcall (cadr (assoc 'get-extra-env-fn build)))
                    make-path output-path)))

(defvar wb-builds
  '((mingw ((ensure-fn wb-mingw-ensure)
            (get-exec-path-fn wb-mingw-get-exec-path)
            (get-path-fn wb-mingw-get-path)
            (get-extra-env-fn wb-mingw-get-extra-env)))
    (msys2-x32 ((ensure-fn wb-msys2-x32-ensure)
                (get-exec-path-fn wb-msys2-get-exec-path)
                (get-path-fn wb-msys2-x32-get-path)
                (get-extra-env-fn wb-msys2-x32-get-extra-env)))
    (msys2-x64 ((ensure-fn wb-msys2-x64-ensure)
                (get-exec-path-fn wb-msys2-get-exec-path)
                (get-path-fn wb-msys2-x64-get-path)
                (get-extra-env-fn wb-msys2-x64-get-extra-env))))
  "List of possbile builds for building Emacs.")

;; * Generic builder
(defun wb-build-full (exec-path path extra-env configuration-dir destination-dir)
  "Build Emacs in CONFIGURATION-DIR from sources in wb-emacs-source and install
it into DESTINATION-DIR.  EXEC-PATH, PATH and EXTRA-ENV would eventually get passed
to wb-command and used there."
  (wb-autogen exec-path path extra-env)
  (wb-configure exec-path path extra-env configuration-dir destination-dir)
  (wb-make exec-path path extra-env configuration-dir)
  (wb-make-install exec-path path extra-env configuration-dir))

(defun wb-autogen (exec-path path extra-env)
  (wb-command exec-path path extra-env "./autogen.sh" wb-emacs-source))

(defun wb-configure (exec-path path extra-env configuration-dir prefix)
  (wb-command exec-path path (append extra-env wb-configure-env)
               (concat "eval " wb-emacs-source "/configure" " \""
                       wb-configure-args " --prefix="
                       (wb-mingw-convert-path prefix) "\"")
               configuration-dir))

(defcustom wb-configure-env '("CFLAGS=-Og -gdwarf-4 -g3")
  "Extra environment vairables to set during configuration.  Compiler optimization is set here.")

(defvar wb-configure-args
  "--without-imagemagick --enable-checking='yes,glyphs' --enable-check-lisp-object-type")

(defun wb-make (exec-path path extra-env configuration-dir)
  (wb-command exec-path path extra-env
               (concat "make -j " (number-to-string wb-make-threads))
               configuration-dir))

(defcustom wb-make-threads 1
  "The number of threads to pass as -j flag to make.")

(defun wb-make-install (exec-path path extra-env configuration-dir)
  (wb-command exec-path path extra-env
               "make install"
               configuration-dir))

(defun wb-command (exec-path path extra-env command &optional dir)
  "Execute shell command COMMAND. Global exec-path is replaced with EXEC-PATH.
EXTRA-ENV is added to process-environment passed to the process.  Path on it
is replaced with PATH.  If DIR is passed, the command is ran in that directory."
  (let* ((shell-file-name "bash")
         ;; By using lexical binding we can use setenv and getenv
         ;; on our local version of process-environment.
         (process-environment (append process-environment extra-env))
         (process-environment (progn (setenv "PATH" path)
                                     ;; Default LANG may screw up automake
                                     ;; version detection in autogen.sh.
                                     (setenv "LANG" "")
                                     process-environment))
         (exec-path exec-path))
    (when dir
      (when (not (file-exists-p dir))
        (mkdir dir t))
      (cd dir))
    (process-file-shell-command command nil "wb")))

;; * MinGW
(defcustom wb-mingw-directory "c:/Emacs/MinGW"
  "* Place to check for MinGW and install it if it's not present."
  :group 'wb
  :type 'directory)

(defun wb-mingw-get-exec-path ()
    (list (concat wb-mingw-directory "/msys/1.0/bin/")))


(defun wb-mingw-convert-path (path)
  "Convert path PATH to MinGW format.  c:/Emacs would become /c/Emacs."
  (concat "/" (replace-regexp-in-string ":" "" path)))

(defun wb-mingw-get-path ()
  (concat "/usr/local/bin/:/mingw/bin/:/bin/:"
          (wb-mingw-convert-path (concat wb-mingw-directory "/mingw32/bin/")) ":"
          (wb-mingw-convert-path (concat wb-mingw-directory "/bin/"))))

(defun wb-mingw-get-extra-env ()
  '())

(defun wb-mingw-ensure ()
  "Ensure we have MinGW installed."
  ;; HACK: need a better check here.
  (when (not (file-exists-p (concat wb-mingw-directory "/msys/1.0/bin/bash.exe")))
    (wb-mingw-install)))

(defun wb-mingw-install ()
  (wb-mingw-install-packages)
  (rename-file (concat wb-mingw-directory "/msys/1.0/etc/" "fstab.sample")
               (concat wb-mingw-directory "/msys/1.0/etc/" "fstab")))

(defun wb-mingw-install-packages ()
  "Install all packages from wb-mingw-packages and wb-msys-packages into wb-mingw-directory."
  (dolist (package wb-mingw-packages)
    (wb-mingw-install-package package wb-mingw-directory))
  (dolist (package wb-msys-packages)
    (wb-mingw-install-package package (concat wb-mingw-directory "/msys/1.0/"))))

(defun wb-mingw-install-package (package path)
  "Install PACKAGE by downloading it and puts it into PATH."
  (wb-7z-extract (wb-wget-download-file package) path t))

(defvar wb-mingw-packages
  '("https://sourceforge.net/projects/mingw/files/MinGW/Base/binutils/binutils-2.25.1/binutils-2.25.1-1-mingw32-bin.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/mingwrt/mingwrt-3.22/mingwrt-3.22.1-mingw32-dev.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/mingwrt/mingwrt-3.22/mingwrt-3.22.1-mingw32-dll.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/w32api/w32api-3.18/w32api-3.18.1-mingw32-dev.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/mpc/mpc-1.0.2/libmpc-1.0.2-mingw32-dll-3.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/mpfr/mpfr-3.1.2-2/mpfr-3.1.2-2-mingw32-dll.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/gmp/gmp-5.1.2/gmp-5.1.2-1-mingw32-dll.tar.lzma"
    ;; Pthreads is no longer required by gcc and does not get installed with it by mingw-get, but it's still required by ld.
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/pthreads-w32/pthreads-w32-2.9.1/pthreads-w32-2.9.1-1-mingw32-dll.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/pthreads-w32/pthreads-w32-2.9.1/pthreads-w32-2.9.1-1-mingw32-dev.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/gettext/gettext-0.18.3.2-2/gettext-0.18.3.2-2-mingw32-dev.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MinGW/Base/gcc/Version5/gcc-5.3.0-2/gcc-core-5.3.0-2-mingw32-bin.tar.xz"
    "https://sourceforge.net/projects/ezwinports/files/pkg-config-0.28-w32-bin.zip"
    ;; gnutls dependencies start
    "https://sourceforge.net/projects/ezwinports/files/p11-kit-0.9-w32-bin.zip"
    "https://sourceforge.net/projects/ezwinports/files/libtasn1-4.2-w32-bin.zip"
    "https://sourceforge.net/projects/ezwinports/files/nettle-2.7.1-w32-bin.zip"
    "https://sourceforge.net/projects/ezwinports/files/zlib-1.2.8-2-w32-bin.zip"
    ;; gnutls dependencies end
    "https://sourceforge.net/projects/ezwinports/files/gnutls-3.3.11-w32-bin.zip"))

(defvar wb-msys-packages
  '("https://sourceforge.net/projects/mingw/files/MSYS/Base/msys-core/msys-1.0.19-1/msysCORE-1.0.19-1-msys-1.0.19-bin.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/bash/bash-3.1.23-1/bash-3.1.23-1-msys-1.0.18-bin.tar.xz"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/gettext/gettext-0.18.1.1-1/libintl-0.18.1.1-1-msys-1.0.17-dll-8.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/libiconv/libiconv-1.14-1/libiconv-1.14-1-msys-1.0.17-dll-2.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/xz/xz-5.0.3-1/liblzma-5.0.3-1-msys-1.0.17-dll-5.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/xz/xz-5.0.3-1/xz-5.0.3-1-msys-1.0.17-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/bzip2/bzip2-1.0.6-1/libbz2-1.0.6-1-msys-1.0.17-dll-1.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/bzip2/bzip2-1.0.6-1/bzip2-1.0.6-1-msys-1.0.17-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/make/make-3.81-3/make-3.81-3-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/coreutils/coreutils-5.97-3/coreutils-5.97-3-msys-1.0.13-ext.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/coreutils/coreutils-5.97-3/coreutils-5.97-3-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/findutils/findutils-4.4.2-2/findutils-4.4.2-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/diffutils/diffutils-2.8.7.20071206cvs-3/diffutils-2.8.7.20071206cvs-3-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/tar/tar-1.23-1/tar-1.23-1-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/less/less-436-2/less-436-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/gawk/gawk-3.1.7-2/gawk-3.1.7-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/gzip/gzip-1.3.12-2/gzip-1.3.12-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/grep/grep-2.5.4-2/grep-2.5.4-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/file/file-5.04-1/libmagic-5.04-1-msys-1.0.13-dll-1.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/file/file-5.04-1/file-5.04-1-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/sed/sed-4.2.1-2/sed-4.2.1-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/regex/regex-1.20090805-2/libregex-1.20090805-2-msys-1.0.13-dll-1.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/termcap/termcap-0.20050421_1-2/termcap-0.20050421_1-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Base/termcap/termcap-0.20050421_1-2/libtermcap-0.20050421_1-2-msys-1.0.13-dll-0.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Extension/flex/flex-2.5.35-2/flex-2.5.35-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Extension/bison/bison-2.4.2-1/bison-2.4.2-1-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Extension/m4/m4-1.4.16-2/m4-1.4.16-2-msys-1.0.17-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Extension/perl/perl-5.8.8-1/perl-5.8.8-1-msys-1.0.17-bin.tar.lzma"
    "https://sourceforge.net/projects/mingw/files/MSYS/Extension/mktemp/mktemp-1.6-2/mktemp-1.6-2-msys-1.0.13-bin.tar.lzma"
    "https://sourceforge.net/projects/ezwinports/files/automake-1.11.6-msys-bin.zip"
    "https://sourceforge.net/projects/ezwinports/files/autoconf-2.65-msys-bin.zip"
    "https://sourceforge.net/projects/ezwinports/files/texinfo-6.1-w32-bin.zip"))


;; * Msys2
(defcustom wb-msys2-x32-force nil
  "Forcefully install 32 bit version of msys2 on 64 it machines."
  :group 'wb)

(defun wb-msys2-install ()
  (let* ((install-x32 (if (or wb-msys2-x32-force
                             (not (wb-windows-is-64-bit))) t nil))
         (dir (if install-x32 wb-msys2-x32-directory wb-msys2-x64-directory))
         (dist (if install-x32 wb-msys2-x32-dist wb-msys2-x64-dist)))
    (wb-7z-extract (wb-wget-download-file dist)
                    (mapconcat 'identity (butlast (split-string dir "/")) "/") t)
    (start-process-shell-command "msys2" "wb" (concat dir "/msys2_shell.cmd"))
    (sleep-for 30)))

(defun wb-msys2-install-packages (exec-path path extra-env packages)
  (dolist (package packages)
    (wb-msys2-install-package exec-path path extra-env package)))

(defun wb-msys2-install-package (exec-path path extra-env package)
  (wb-command exec-path path extra-env
               (concat "pacman -S --noconfirm --needed " package)))

(defun wb-windows-is-64-bit ()
  "Determines whether Windows is 64 bit."
  ;; HACK, but should generally work.
  (file-exists-p "c:/Program Files (x86)/"))

(defun wb-msys2-get-common-path ()
  (concat "/usr/local/bin:/usr/bin:"
          "/bin:/c/Windows/System32:/c/Windows:"
          "/c/Windows/System32/Wbem:/c/Windows/System32/WindowsPowerShell/v1.0/:"
          "/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/"))

(defun wb-msys2-get-exec-path ()
  (list (concat (wb-msys2-get-current-directory)  "/usr/bin/")))

(defun wb-msys2-get-current-directory ()
  "Return directory for currently installed msys"
  (if (or wb-msys2-x32-force
          (not (wb-windows-is-64-bit))) wb-msys2-x32-directory
    wb-msys2-x64-directory))

;; ** x32
(defcustom wb-msys2-x32-directory "c:/Emacs/msys32"
  "* Place to check for 32 bit msys2 and install it if it's not present."
  :group 'wb
  :type 'directory)

(defun wb-msys2-x32-get-path ()
  (concat "/mingw32/bin:" (wb-msys2-get-common-path)))

(defun wb-msys2-x32-get-extra-env ()
  '("MSYSTEM=MINGW32" "PKG_CONFIG_PATH=/mingw32/lib/pkgconfig"))

(defun wb-msys2-x32-ensure ()
  ;; Need a much better check here...
  (when (not (file-exists-p (wb-msys2-get-current-directory)))
    (wb-msys2-install))
  (wb-msys2-install-packages (wb-msys2-get-exec-path) (wb-msys2-x32-get-path)
                              (wb-msys2-x32-get-extra-env) wb-msys2-x32-packages))

(defvar wb-msys2-x32-packages '("base-devel" "mingw-w64-i686-toolchain"
                                 "mingw-w64-i686-xpm-nox" "mingw-w64-i686-libtiff"
                                 "mingw-w64-i686-giflib" "mingw-w64-i686-libpng"
                                 "mingw-w64-i686-libjpeg-turbo" "mingw-w64-i686-librsvg"
                                 "mingw-w64-i686-libxml2" "mingw-w64-i686-gnutls"))

(defvar wb-msys2-x32-dist
  "https://sourceforge.net/projects/msys2/files/Base/i686/msys2-base-i686-20160719.tar.xz")

;; ** x64
(defcustom wb-msys2-x64-directory "c:/Emacs/msys64"
  "* Place to check for MinGW and install it if it's not present."
  :group 'wb
  :type 'directory)

(defun wb-msys2-x64-get-path ()
  (concat "/mingw64/bin:" (wb-msys2-get-common-path)))


(defun wb-msys2-x64-get-extra-env ()
  '("MSYSTEM=MINGW64" "PKG_CONFIG_PATH=/mingw64/lib/pkgconfig"))

(defun wb-msys2-x64-ensure ()
  ;; Need a much better check here...
  (when (not (file-exists-p wb-msys2-x64-directory))
    (wb-msys2-install))
  (wb-msys2-install-packages (wb-msys2-get-exec-path) (wb-msys2-x64-get-path)
                              (wb-msys2-x64-get-extra-env) wb-msys2-x64-packages))

(defvar wb-msys2-x64-packages '("base-devel" "mingw-w64-x86_64-toolchain"
                                 "mingw-w64-x86_64-xpm-nox" "mingw-w64-x86_64-libtiff"
                                 "mingw-w64-x86_64-giflib" "mingw-w64-x86_64-libpng"
                                 "mingw-w64-x86_64-libjpeg-turbo" "mingw-w64-x86_64-librsvg"
                                 "mingw-w64-x86_64-libxml2" "mingw-w64-x86_64-gnutls"))

(defvar wb-msys2-x64-dist
  "https://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-base-x86_64-20160719.tar.xz")

;; * 7zip
(defun wb-7z-extract (file path &optional keep)
  "Recursively extracts archives."
  (let* ((file-list (reverse (split-string file "\\.")))
         (recurse (member (cadr file-list) wb-7z-archives-to-recurse))
         (extract-path (if recurse (file-name-directory file) path))
         (new-file (substring file 0 (- (+ 1 (string-width (car file-list)))))))
    (process-file-shell-command
     (concat "\"" (wb-get-7z) "\" x " file " -aoa -o"
             (replace-regexp-in-string "/" "\\\\" extract-path)) nil "wb")
        (when (not keep) (delete-file file))
    (when recurse (wb-7z-extract new-file path))))

(defvar wb-7z-archives-to-recurse '("tar" "lzma"))

(defun wb-get-7z ()
  "Ensure we have 7zip on our path for unarchiving."
  (or (executable-find "7z.exe")
      (locate-file "7z.exe" wb-7z-paths)
      (wb-install-7z)))

(defun wb-install-7z ()
  (let ((setup-file (wb-wget-download-file (if (wb-windows-is-64-bit)
                                                wb-7z-x64-setup
                                              wb-7z-x32-setup)))
        (setup-dir (concat "\""
                           (replace-regexp-in-string
                            "/"
                            "\\\\"
                            "c:/Program Files/7-Zip/")
                           "\"")))
  (process-file-shell-command
   (concat setup-file " /S  /D=" setup-dir) nil "wb")
  (locate-file "7z.exe" wb-7z-paths)))

(defvar wb-7z-paths '("c:/Program Files/7-Zip/" "c:/Program Files (x86)/7-Zip/"))

(defvar wb-7z-x64-setup "http://www.7-zip.org/a/7z1602-x64.exe")

(defvar wb-7z-x32-setup "http://www.7-zip.org/a/7z1602.exe")

;; * Wget
(defun wb-wget-download-file (file)
  (let* ((local-file (concat wb-wget-download-directory "/"
                             (car (reverse (split-string file "/")))))
         (wget (wb-wget-get))
         (check-certificate (if (wb-wget-check-certificate)
                                "--no-check-certificate" "")))
    (when (not (file-exists-p local-file))
      (when (not (file-exists-p wb-wget-download-directory))
        (mkdir wb-wget-download-directory t))
      (cd wb-wget-download-directory)
      (process-file-shell-command
       (concat "\"" wget
               "\" " check-certificate " " file) nil "wb"))
    local-file))

(defun wb-wget-get ()
  "Ensure we have wget on our path for downloading dependencies.
Wget works nicely, since it's able to get stuff from sourceforge."
  (let ((wget (or (executable-find "wget.exe")
              (locate-file "wget.exe" wb-wget-paths)
              (error "Wget not found."))))
    wget))

(defun wb-wget-check-certificate ()
  "Whether to pass --no-check-certificate flag to wget.
Needed for GnuWin version, because it fails for https."
  (let ((wget (wb-wget-get)))
    (if (string-match "GnuWin32" wget)
        t
      nil)))

(defcustom wb-wget-paths '("c:/Program Files (x86)/GnuWin32/bin/" "c:/Program Files/GnuWin32/bin/")
  "*Paths to search for wget."
  :group 'wb)

(defcustom wb-wget-download-directory "c:/Emacs/downloads"
  "*Directory to put files downloaded by wget."
  :group 'wb
  :type 'directory)

(provide 'windows-builder)
;;; windows-builder.el ends here

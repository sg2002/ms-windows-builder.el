;;; ms-windows-builder-config.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Nikolay Kudryavtsev <Nikolay.Kudryavtsev@gmail.com>

;; Author: Nikolay Kudryavtsev <Nikolay.Kudryavtsev@gmail.com>
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

;; This file contains variables used for configuring ms-windows-builder.

;;; Code:
(defcustom mwb-emacs-source "c:/Emacs/source"
  "*Directory that contains Emacs source code."
  :group 'mwb
  :type 'directory)

(defcustom mwb-mingw-directory "c:/Emacs/MinGW"
  "* Place to check for MinGW and install it if it's not present."
  :group 'mwb
  :type 'directory)

(defcustom mwb-msys2-x32-directory "c:/Emacs/msys32"
  "* Place to check for 32 bit msys2 and install it if it's not present."
  :group 'mwb
  :type 'directory)

(defcustom mwb-msys2-x64-directory "c:/Emacs/msys64"
  "* Place to check for MinGW and install it if it's not present."
  :group 'mwb
  :type 'directory)

(defcustom mwb-wget-download-directory "c:/Emacs/downloads"
  "*Directory to put files downloaded by wget."
  :group 'mwb
  :type 'directory)

(defvar mwb-toolchains
  '((mingw ((ensure-fn mwb-mingw-ensure)
            (get-exec-path-fn mwb-mingw-get-exec-path)
            (get-path-fn mwb-mingw-get-path)
            (get-extra-env-fn mwb-mingw-get-extra-env)
            (get-libraries-dir-fn mwb-mingw-get-libraries-dir)))
    (msys2-x32 ((ensure-fn mwb-msys2-x32-ensure)
                (get-exec-path-fn mwb-msys2-get-exec-path)
                (get-path-fn mwb-msys2-x32-get-path)
                (get-extra-env-fn mwb-msys2-x32-get-extra-env)
                (get-libraries-dir-fn mwb-msys2-get-libraries-dir)))
    (msys2-x64 ((ensure-fn mwb-msys2-x64-ensure)
                (get-exec-path-fn mwb-msys2-get-exec-path)
                (get-path-fn mwb-msys2-x64-get-path)
                (get-extra-env-fn mwb-msys2-x64-get-extra-env)
                (get-libraries-dir-fn mwb-msys2-get-libraries-dir))))
  "List of possbile builds for building Emacs.")

(defcustom mwb-configurations
  '((debug
     ((configure-env ("CFLAGS=-O0 -gdwarf-2 -g3"))
      (configure-args ("--without-imagemagick"
                       "--with-wide-int"
                       "--enable-checking='yes,glyphs'"
                       "--enable-check-lisp-object-type"))))
    (debug-with-modules
     ((configure-env ("CFLAGS=-O0 -gdwarf-2 -g3"))
      (configure-args ("--without-imagemagick"
                       "--with-wide-int"
                       "--enable-checking='yes,glyphs'"
                       "--enable-check-lisp-object-type"
                       "--with-modules"))))
    (release
     ((configure-env ("CFLAGS=-O2 -gdwarf-4 -g3"))
      (configure-args ("--without-imagemagick"
                       "--with-wide-int"))
      (install-strip t)))
    (release-with-modules
     ((configure-env ("CFLAGS=-O2 -gdwarf-4 -g3"))
      (configure-args ("--without-imagemagick"
                       "--with-wide-int"
                       "--with-modules"))
      (install-strip t))))
  "*List of possible configurations."
  :group 'mwb)

(defcustom mwb-default-configuration 'debug
  "*Default configure setup to use."
  :group 'mwb)

(defcustom mwb-configuration-args
  '(("--with-wide-int" ((toolchains (mingw msys2-x32)))))
  "*Possible options for a specific configuration argument.
Currently it only allows to limit use of specific arguments by toolchains."
  :group 'mwb)

(defcustom mwb-make-threads 1
  "The number of threads to pass as -j flag to make.")

(defvar mwb-mingw-packages
  '(("https://sourceforge.net/projects/mingw/files/MinGW/Base/"
     ("binutils/binutils-2.25.1/binutils-2.25.1-1-mingw32-bin.tar.xz"
      ;;"mingwrt/mingwrt-3.22/mingwrt-3.22.4-mingw32-dll.tar.xz"
      ;;"mingwrt/mingwrt-3.22/mingwrt-3.22.4-mingw32-dev.tar.xz"
      ;;"w32api/w32api-3.18/w32api-3.18.2-mingw32-dev.tar.xz"
      ;; Building with libxml and glib(both required for svg image support)
      ;; would fail with newer mingwrt and w32api.
      ;; This was fixed in 34b6df1, but if you're building up to and including 25.1
      ;; use these older versions:
      "mingwrt/mingwrt-3.21.1/mingwrt-3.21.1-mingw32-dll.tar.xz"
      "mingwrt/mingwrt-3.21.1/mingwrt-3.21.1-mingw32-dev.tar.xz"
      "w32api/w32api-3.17/w32api-3.17-2-mingw32-dev.tar.lzma"
      "mpc/mpc-1.0.2/libmpc-1.0.2-mingw32-dll-3.tar.xz"
      "mpfr/mpfr-3.1.2-2/mpfr-3.1.2-2-mingw32-dll.tar.lzma"
      "gmp/gmp-5.1.2/gmp-5.1.2-1-mingw32-dll.tar.lzma"
      ;; Pthreads is no longer required by gcc and does not get installed with it by mingw-get, but it's still required by ld.
      "pthreads-w32/pthreads-w32-2.9.1/pthreads-w32-2.9.1-1-mingw32-dll.tar.lzma"
      "pthreads-w32/pthreads-w32-2.9.1/pthreads-w32-2.9.1-1-mingw32-dev.tar.lzma"
      "gettext/gettext-0.18.3.2-2/gettext-0.18.3.2-2-mingw32-dev.tar.xz"
      "gcc/Version5/gcc-5.3.0-2/gcc-core-5.3.0-2-mingw32-bin.tar.xz"
      ;; Newer libiconv is available, but we still use this one, because that's
      ;; what ezwinports libxml is linked against.
      "libiconv/libiconv-1.13.1-1/libiconv-1.13.1-1-mingw32-dev.tar.lzma"))
    ("https://sourceforge.net/projects/ezwinports/files/"
     ("pkg-config-0.28-w32-bin.zip"
      "zlib-1.2.8-2-w32-bin.zip"
      ;; Gnutls
      "p11-kit-0.9-w32-bin.zip"
      "libidn-1.29-w32-bin.zip"
      "libtasn1-4.9-w32-bin.zip"
      "nettle-3.3-w32-bin.zip"
      "gnutls-3.4.15-w32-bin.zip"
      ;; Images
      "giflib-5.1.0-w32-bin.zip"
      "jpeg-v9a-w32-bin.zip"
      "libpng-1.6.12-w32-bin.zip"
      "tiff-4.0.3-w32-bin.zip"
      "libXpm-3.5.11-2-w32-bin.zip"
      "pcre-8.21-w32-bin.zip"
      "glib-2.38.2-w32-bin.zip"
      "bzip2-1.0.6-w32-bin.zip"
      "pixman-0.32.4-w32-bin.zip"
      "cairo-1.12.16-w32-bin.zip"
      "libcroco-0.6.8-w32-bin.zip"
      "gdk-pixbuf-2.30.2-w32-bin.zip"
      "pango-1.36.1-2-w32-bin.zip"
      "librsvg-2.40.1-2-w32-bin.zip"))))


(defvar mwb-msys-packages
  '(("https://sourceforge.net/projects/mingw/files/MSYS/"
     ("Base/msys-core/msys-1.0.19-1/msysCORE-1.0.19-1-msys-1.0.19-bin.tar.xz"
      ;; msys.ext is not necessary, but it contains msys.bat
      "Base/msys-core/msys-1.0.19-1/msysCORE-1.0.19-1-msys-1.0.19-ext.tar.xz"
      "Base/bash/bash-3.1.23-1/bash-3.1.23-1-msys-1.0.18-bin.tar.xz"
      "Base/gettext/gettext-0.18.1.1-1/libintl-0.18.1.1-1-msys-1.0.17-dll-8.tar.lzma"
      "Base/libiconv/libiconv-1.14-1/libiconv-1.14-1-msys-1.0.17-dll-2.tar.lzma"
      "Base/xz/xz-5.0.3-1/liblzma-5.0.3-1-msys-1.0.17-dll-5.tar.lzma"
      "Base/xz/xz-5.0.3-1/xz-5.0.3-1-msys-1.0.17-bin.tar.lzma"
      "Base/bzip2/bzip2-1.0.6-1/libbz2-1.0.6-1-msys-1.0.17-dll-1.tar.lzma"
      "Base/bzip2/bzip2-1.0.6-1/bzip2-1.0.6-1-msys-1.0.17-bin.tar.lzma"
      "Base/make/make-3.81-3/make-3.81-3-msys-1.0.13-bin.tar.lzma"
      "Base/coreutils/coreutils-5.97-3/coreutils-5.97-3-msys-1.0.13-ext.tar.lzma"
      "Base/coreutils/coreutils-5.97-3/coreutils-5.97-3-msys-1.0.13-bin.tar.lzma"
      "Base/findutils/findutils-4.4.2-2/findutils-4.4.2-2-msys-1.0.13-bin.tar.lzma"
      "Base/diffutils/diffutils-2.8.7.20071206cvs-3/diffutils-2.8.7.20071206cvs-3-msys-1.0.13-bin.tar.lzma"
      "Base/tar/tar-1.23-1/tar-1.23-1-msys-1.0.13-bin.tar.lzma"
      "Base/less/less-436-2/less-436-2-msys-1.0.13-bin.tar.lzma"
      "Base/gawk/gawk-3.1.7-2/gawk-3.1.7-2-msys-1.0.13-bin.tar.lzma"
      "Base/gzip/gzip-1.3.12-2/gzip-1.3.12-2-msys-1.0.13-bin.tar.lzma"
      "Base/grep/grep-2.5.4-2/grep-2.5.4-2-msys-1.0.13-bin.tar.lzma"
      "Base/file/file-5.04-1/libmagic-5.04-1-msys-1.0.13-dll-1.tar.lzma"
      "Base/file/file-5.04-1/file-5.04-1-msys-1.0.13-bin.tar.lzma"
      "Base/sed/sed-4.2.1-2/sed-4.2.1-2-msys-1.0.13-bin.tar.lzma"
      "Base/regex/regex-1.20090805-2/libregex-1.20090805-2-msys-1.0.13-dll-1.tar.lzma"
      "Base/termcap/termcap-0.20050421_1-2/termcap-0.20050421_1-2-msys-1.0.13-bin.tar.lzma"
      "Base/termcap/termcap-0.20050421_1-2/libtermcap-0.20050421_1-2-msys-1.0.13-dll-0.tar.lzma"
      "Extension/flex/flex-2.5.35-2/flex-2.5.35-2-msys-1.0.13-bin.tar.lzma"
      "Extension/bison/bison-2.4.2-1/bison-2.4.2-1-msys-1.0.13-bin.tar.lzma"
      "Extension/m4/m4-1.4.16-2/m4-1.4.16-2-msys-1.0.17-bin.tar.lzma"
      ;; Perl dependencies start. Perl is needed for automake.
      "Extension/libxml2/libxml2-2.7.6-1/libxml2-2.7.6-1-msys-1.0.13-dll-2.tar.lzma"
      "Extension/expat/expat-2.0.1-1/libexpat-2.0.1-1-msys-1.0.13-dll-1.tar.lzma"
      "Extension/crypt/crypt-1.1_1-3/libcrypt-1.1_1-3-msys-1.0.13-dll-0.tar.lzma"
      "Extension/gdbm/gdbm-1.8.3-3/libgdbm-1.8.3-3-msys-1.0.13-dll-3.tar.lzma"
      "Extension/perl/perl-5.8.8-1/perl-5.8.8-1-msys-1.0.17-bin.tar.lzma"
      "Extension/mktemp/mktemp-1.6-2/mktemp-1.6-2-msys-1.0.13-bin.tar.lzma"))
    ("https://sourceforge.net/projects/ezwinports/files/"
     ("automake-1.11.6-msys-bin.zip"
      "autoconf-2.65-msys-bin.zip"
      "texinfo-6.3-w32-bin.zip"))))


(defvar mwb-msys2-x32-packages '("base-devel" "mingw-w64-i686-toolchain"
                                 "mingw-w64-i686-libxml2" "mingw-w64-i686-gnutls"
                                 "mingw-w64-i686-xpm-nox" "mingw-w64-i686-libtiff"
                                 "mingw-w64-i686-giflib" "mingw-w64-i686-libpng"
                                 "mingw-w64-i686-libjpeg-turbo" "mingw-w64-i686-librsvg"))

(defvar mwb-msys2-x32-dist
  "https://sourceforge.net/projects/msys2/files/Base/i686/msys2-base-i686-20161025.tar.xz")


(defvar mwb-msys2-x64-packages '("base-devel" "mingw-w64-x86_64-toolchain"
                                 "mingw-w64-x86_64-libxml2" "mingw-w64-x86_64-gnutls"
                                 "mingw-w64-x86_64-xpm-nox" "mingw-w64-x86_64-libtiff"
                                 "mingw-w64-x86_64-giflib" "mingw-w64-x86_64-libpng"
                                 "mingw-w64-x86_64-libjpeg-turbo" "mingw-w64-x86_64-librsvg"))

(defvar mwb-msys2-x64-dist
  "https://sourceforge.net/projects/msys2/files/Base/x86_64/msys2-base-x86_64-20161025.tar.xz")

(defcustom mwb-dynamic-libraries
  '(;; libwinpthread is needed for msys2 only, it can be linked statically
    ;; by passing CFLAGS= -static to the configure script.
    "libwinpthread-.*\\.dll"
    "libgcc_s_seh-.*\\.dll"
    ;; "libdbus-.*\\.dll"
    "zlib.*\\.dll" ; used by emacs for compression, also by libcroco
    "liblzma-.*\\.dll" ;; required by libxml2 and libtiff
    ;; XML support library, required for HTML and XML support in Emacs
    ;; also required by rsvg
    "libxml2-.*\\.dll"
    ;; Gnutls
    "libffi-.*\\.dll" ; only needed for msys2-based builds
    "libgmp-.*\\.dll" "libgnutls-[0-9].\\.dll" "libhogweed-.*\\.dll"
    "libiconv-.*\\.dll" "libidn-.*\\.dll" "libintl-.*\\.dll"
    "libnettle-.*\\.dll" "libp11-kit-.*\\.dll" "libtasn1-.*\\.dll"
    ;; Images
    "libgif-.*\\.dll" ; gif images
    "libjpeg-.*\\.dll" ; jpeg images
    "libpng.*\\.dll" ; png images, also required by libcroco and libgdk_pixbuf
    "libtiff-.*\\.dll" ; tiff images
    "libXpm-noX.*\\.dll" ; xpm images
    ;; svg images
    "libpcre-.*\\.dll" "libglib-.*\\.dll" "libgmodule-.*\\.dll" "libgobject-.*\\.dll"
    "libgio-.*\\.dll" "libexpat-.*\\.dll" "libfontconfig-.*\\.dll""libbz2-.*\\.dll"
    "libstdc\\+\\+-.*\\.dll" "libgraphite.*\\.dll" "libharfbuzz-.\\.dll" "libfreetype-.*\\.dll"
    "libpixman-.*\\.dll" "libcairo-.\\.dll" "libcroco-.*\\.dll" "libgdk_pixbuf-.*\\.dll"
    "libpango-.*\\.dll" "libpangoft.*\\.dll" "libpangowin32-.*\\.dll" "libpangocairo-.*\\.dll"
    "librsvg-.*\\.dll")
  "Dynamic libraries to copy into the installation dir.")

(provide 'ms-windows-builder-config)
;;; ms-windows-builder-config.el ends here

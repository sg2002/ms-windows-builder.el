;;; windows-builder.el --- Elisp script for quckly building emacs on Windows.  -*- lexical-binding: t; -*-

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

;;; Code:
(require 'ms-windows-builder-config)

;; * Main
(defun mwb-build (selected-toolchain output-directory &optional configuration)
  "Build Emacs using SELECTED-TOOLCHAIN, which should be defined in mwb-builds.
Install Emacs into OUTPUT-DIRECTORY  If CONFIGURATION is specified use it,
otherwise use mwb-default-configuration.  CONFIGURATION should be defined
in mwb-configurations."
  (let ((configuration-directory (concat (file-name-as-directory mwb-configurations-directory)
                           (file-name-nondirectory (directory-file-name output-directory))))
        (fininshed-fn (mwb-start))
        (toolchain (cadr (assoc selected-toolchain mwb-toolchains)))
        (selected-configuration
         (mwb-apply-arg-configurations
          selected-toolchain
          (cadr (assoc (if configuration configuration
                         mwb-default-configuration)
                       mwb-configurations)))))
    (funcall (cadr (assoc 'ensure-fn toolchain))
             (lambda ()
               (mwb-build-full
                selected-toolchain
                (funcall (cadr (assoc 'get-exec-path-fn toolchain)))
                (funcall (cadr (assoc 'get-path-fn toolchain)))
                (funcall (cadr (assoc 'get-extra-env-fn toolchain)))
                selected-configuration configuration-directory output-directory
                (funcall (cadr (assoc 'get-libraries-dir-fn toolchain)))
                (funcall (cadr (assoc 'get-libraries-fn toolchain)))
                fininshed-fn)))))

(defun mwb-apply-arg-configurations (selected-toolchain configuration)
  "Apply configurations for each configure argument in the configuration."
  (add-to-list
   'configuration
   `(configure-args .
                    ,(list (mwb-filter-args selected-toolchain
                                            (cadr (assoc 'configure-args configuration)))))))

(defun mwb-filter-args (selected-toolchain configure-args)
  "Filter arguments from CONFIGURE-ARGS, when SELECTED-TOOLCHAIN is in
mwb-confugration-args for them."
  (delq
   nil
   (mapcar
    (lambda (s)
      (if
          (or
           (not (assoc 'toolchains
                       (cadr (assoc s mwb-configuration-args))))
           (memq selected-toolchain
                 (cadr (assoc 'toolchains
                              (cadr (assoc s mwb-configuration-args))))))
          s nil))
    configure-args)))

(defvar mwb-started nil)

(defun mwb-start(&optional continue)
  (if (and (null continue) mwb-started)
      (error "mwb: Build operation is already running. You can stop it using mwb-stop.")
    (progn (setq mwb-started t)
           (mwb-get-fininshed-fn))))

(defun mwb-stop ()
  (interactive)
  (setq mwb-started nil)
  (when (get-process "mwb")
    (delete-process "mwb")))

(defun mwb-get-fininshed-fn ()
  (let ((start-time (current-time)))
    (lambda () (setq mwb-started nil)
      (let ((msg (format-message "mwb: Operation finished after: %s"
                          (format-time-string "%T"
                                              (time-subtract (current-time) start-time) t)))
            (inhibit-read-only t))
        (with-current-buffer (mwb-get-buffer)
          (insert msg))
        (message msg)))))

;; * Generic builder

(defun mwb-build-full (toolchain exec-path path extra-env configuration
                                 configuration-dir destination-dir libraries-dir
                                 libraries
                                 &optional finished-fn)
  "Build Emacs in CONFIGURATION-DIR from sources in mwb-emacs-source and install
it into DESTINATION-DIR.  EXEC-PATH, PATH and EXTRA-ENV would eventually get passed
to mwb-command and used there."
  (let ((temp-destination-dir
         (if (not (mwb-is-cygwin toolchain))
             (make-temp-name destination-dir)
           destination-dir)))
    (mwb-thread-cps
     (mwb-autogen exec-path path extra-env)
     (mwb-configure toolchain exec-path path extra-env configuration configuration-dir temp-destination-dir)
     (mwb-make exec-path path extra-env configuration-dir)
     (mwb-make-install exec-path path extra-env configuration configuration-dir)
     (mwb-copy-libraries libraries libraries-dir temp-destination-dir)
     (mwb-replace-destination toolchain destination-dir temp-destination-dir)
     (funcall finished-fn))))

(defmacro mwb-thread-cps (&rest forms)
  "Thread FORMS elements wrapping each subsequent form into a lambda
passed as the last argument into a prior form."
  `(funcall (mwb-thread-cps-impl ,(reverse forms))))

(defmacro mwb-thread-cps-impl (forms &optional k)
  "Internal implementation for `mwb-thread-cps'."
  (pcase forms
    (`(,f . ,rest)
     `(mwb-thread-cps-impl ,rest
                          (lambda ()
                            ,(if k
                                 (append f (list k))
                               f))))
    (_ k)))

(defun mwb-autogen (exec-path path extra-env k)
  (mwb-command exec-path path extra-env "./autogen.sh" mwb-emacs-source k))

(defun mwb-configure (toolchain exec-path path extra-env configuration configuration-dir prefix k)
  (let*((cygwin-paths (mwb-is-cygwin toolchain))
        (source-path (if cygwin-paths (mwb-cygwin-convert-path mwb-emacs-source)
                       (mwb-mingw-convert-path mwb-emacs-source)))
        (prefix-path (if cygwin-paths (mwb-cygwin-convert-path prefix)
                       (mwb-mingw-convert-path prefix))))
    (mwb-command exec-path path (append extra-env (cadr (assoc 'configure-env configuration)))
                 (concat "eval " source-path "configure" " \""
                         (mapconcat 'identity (cadr (assoc 'configure-args configuration)) " ")
                         " --prefix="
                         prefix-path "\"")
                 configuration-dir k)))

(defun mwb-make (exec-path path extra-env configuration-dir k)
  (mwb-command exec-path path extra-env
               (concat "make -j " (number-to-string mwb-make-threads))
               configuration-dir k))

(defun mwb-make-install (exec-path path extra-env configuration configuration-dir k)
  (mwb-command exec-path path extra-env
               (concat "make install"
                       (when (cadr (assoc 'install-strip configuration))
                         "-strip"))
               configuration-dir k))

(defun mwb-copy-libraries (libraries libraries-dir destination-dir &optional k)
  "Copies each library from mwb-dynamic-libraries that exists in LIBRARIES-DIR
into DESTINATION-DIR."
  (dolist (library libraries)
    (dolist (library-file (directory-files libraries-dir t library))
      (copy-file library-file (concat (file-name-as-directory destination-dir) "bin") t)))
  (when k (funcall k)))

(defun mwb-replace-destination (toolchain destination-dir temp-destination-dir &optional k)
  "Move Emacs into the final destination.
Check if DESTINATION-DIR already contains Emacs and that Emacs is not currently
running.  If so, replace it with a newly built one from TEMP-DESTINATION-DIR.
Then call continuation K."
  (when (not (mwb-is-cygwin toolchain))
    (when (file-exists-p destination-dir)
      (condition-case err
          (let* ((bin-name (concat (file-name-as-directory destination-dir) "bin"))
                 (temp-name (make-temp-name bin-name)))
            (rename-file bin-name temp-name)
            (rename-file temp-name bin-name))
        (error (message "Could not replace Emacs in %s with newly built Emacs in %s"
                        destination-dir temp-destination-dir))))
    (copy-directory temp-destination-dir destination-dir t t t)
    (delete-directory temp-destination-dir t))
  (when k
    (funcall k)))

(defun mwb-get-sentinel(k)
  (lambda (process event)
    (if (and (not (null mwb-started)) (equal event "finished\n"))
        (funcall k)
      (progn
        (message "mwb: Build operation failed. See mwb buffer for more details.")
        (setq mwb-started nil)))))

(defun mwb-command (exec-path path extra-env command &optional dir k)
  "Execute shell command COMMAND. Global exec-path is replaced with EXEC-PATH.
EXTRA-ENV is added to process-environment passed to the process.  Path on it
is replaced with PATH.  If DIR is passed, the command is ran in that directory."
  (with-current-buffer (mwb-get-buffer)
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
     (let ((process (start-file-process-shell-command "mwb"
                                                      (current-buffer) command)))
       (set-process-sentinel process (mwb-get-sentinel k))))))

(defun mwb-get-buffer ()
  (let ((buffer (get-buffer-create "mwb")))
    (with-current-buffer buffer
      (when (not (eq major-mode 'compilation-mode))
          (compilation-mode)))
    buffer))

;; * MinGW
(defun mwb-mingw-get-exec-path ()
    (list (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/bin/")))


(defun mwb-mingw-convert-path (path)
  "Convert path PATH to MinGW format.  c:/Emacs would become /c/Emacs."
  (concat "/" (replace-regexp-in-string ":" "" path)))

(defun mwb-mingw-get-path ()
  (concat "/usr/local/bin/:/mingw/bin/:/bin/:"
          (mwb-mingw-convert-path
           (concat (file-name-as-directory mwb-mingw-directory) "mingw32/bin/")) ":"
           (mwb-mingw-convert-path
            (concat (file-name-as-directory mwb-mingw-directory) "bin/"))))

(defun mwb-mingw-get-extra-env ()
  '())

(defun mwb-mingw-get-libraries-dir ()
  (concat (file-name-as-directory mwb-mingw-directory) "bin/"))

(defun mwb-mingw-get-libraries ()
  "Return list of libraries to copy into bin."
  mwb-dynamic-libraries)

(defun mwb-mingw-ensure (k)
  "Ensure we have MinGW installed."
  ;; HACK: need a better check here.
  (if (not (file-exists-p (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/bin/bash.exe")))
      (mwb-mingw-install k)
    (funcall k)))

(defun mwb-mingw-install (&optional k)
  (let ((k (if k k (mwb-start))))
    (mwb-mingw-install-packages (lambda () (mwb-mingw-post-extract k)))))

(defun mwb-mingw-post-extract (k)
  (rename-file (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/etc/" "fstab.sample")
               (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/etc/" "fstab"))
  (with-current-buffer (find-file-noselect
                        (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/etc/" "fstab") t)
    (replace-string "c:/MinGW" (file-name-as-directory mwb-mingw-directory))
    (save-buffer)
    (kill-buffer))
  (when k
    (funcall k)))

(defun mwb-mingw-install-packages (&optional k)
  "Install all packages from mwb-mingw-packages and mwb-msys-packages into mwb-mingw-directory."
  (let ((k (if k k (mwb-start)))
        (packages '()))
    (dolist (source-packages mwb-mingw-packages)
      (setq packages (append packages (mwb-mingw-packages-from-source-packages source-packages mwb-mingw-directory))))
    (dolist (source-packages mwb-msys-packages)
      (setq packages
            (append packages
                    (mwb-mingw-packages-from-source-packages
                     source-packages
                     (concat (file-name-as-directory mwb-mingw-directory) "msys/1.0/")))))
    (funcall (mwb-mingw-install-packages-cps (reverse packages) k))))


(defun mwb-mingw-packages-from-source-packages (source-packages directory)
  "Install packages from a list SOURCE-PACKAGES into DIRECTORY.
SOURCE-PACKAGES should have the common download path as car and the list of packages as cdr."
  (mapcar (lambda (package)
            (cons (concat (car source-packages) package) directory))
          (cadr source-packages)))

(defun mwb-mingw-install-packages-cps (packages k)
  (if (eq nil packages) k
    (mwb-mingw-install-packages-cps (cdr packages)
                                    (lambda ()
                                      (mwb-mingw-install-package (car (car packages))
                                                                 (cdr (car packages)) k)))))

(defun mwb-mingw-install-package (package path k)
  "Install PACKAGE by downloading it and puts it into PATH."
  (mwb-wget-download-file package (lambda (file) (mwb-extract file path k))))

;; * Msys2
(defcustom mwb-msys2-x32-force nil
  "Forcefully install 32 bit version of msys2 on 64 it machines."
  :group 'mwb)

(defun mwb-msys2-install-base (k)
  (let* ((install-x32 (if (or mwb-msys2-x32-force
                             (not (mwb-windows-is-64-bit))) t nil))
         (dir (file-name-directory (if install-x32 mwb-msys2-x32-directory mwb-msys2-x64-directory)))
         (dist (if install-x32 mwb-msys2-x32-dist mwb-msys2-x64-dist)))
    (mwb-wget-download-file
     dist
     (lambda (file)
       (mwb-extract file
                    (mapconcat 'identity
                               (butlast (split-string (directory-file-name dir) "/")) "/")
                               (lambda ()
                                 (start-process-shell-command "msys2" "mwb" (concat dir "msys2_shell.cmd"))
                                 (run-at-time "30 sec" nil k)))))))

(defun mwb-msys2-install-packages (exec-path path extra-env packages &optional k)
  (let ((k (if k k (mwb-start))))
        (funcall (mwb-msys2-install-packages-cps exec-path path extra-env (reverse packages) k))))

(defun mwb-msys2-install-packages-cps (exec-path path extra-env packages k)
  (if (eq nil packages) k
    (mwb-msys2-install-packages-cps
     exec-path path extra-env
     (cdr packages)
     (lambda ()
       (mwb-msys2-install-package exec-path path extra-env (car packages) k)))))

(defun mwb-msys2-install-package (exec-path path extra-env package k)
  (mwb-command exec-path path extra-env
               (concat "pacman -S --noconfirm --needed " package) nil k))

(defun mwb-windows-is-64-bit ()
  "Determines whether Windows is 64 bit."
  ;; HACK, but should generally work.
  (file-exists-p "c:/Program Files (x86)/"))

(defun mwb-msys2-get-common-path ()
  (concat "/usr/local/bin:/usr/bin:"
          "/bin:/c/Windows/System32:/c/Windows:"
          "/c/Windows/System32/Mwbem:/c/Windows/System32/WindowsPowerShell/v1.0/:"
          "/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/"))

(defun mwb-msys2-get-exec-path ()
  (list (concat (mwb-msys2-get-current-directory)  "usr/bin/")))

(defun mwb-msys2-get-current-directory ()
  "Return directory for currently installed msys"
  (file-name-as-directory
   (if (or mwb-msys2-x32-force
           (not (mwb-windows-is-64-bit))) mwb-msys2-x32-directory
     mwb-msys2-x64-directory)))

(defun mwb-msys2-get-libraries ()
  "Return list of libraries to copy into bin."
  (append mwb-dynamic-libraries mwb-msys2-dynamic-libraries))

;; ** x64
(defun mwb-msys2-x64-get-path ()
  (concat "/mingw64/bin:" (mwb-msys2-get-common-path)))


(defun mwb-msys2-x64-get-extra-env ()
  '("MSYSTEM=MINGW64"))

(defun mwb-msys2-x64-ensure (k)
  (if (not (file-exists-p mwb-msys2-x64-directory))
      (mwb-msys2-x64-install k)
    (funcall (mwb-msy2-x64-get-package-install-fn k))))

(defun mwb-msys2-x64-install (&optional k)
  (let ((k (if k k (mwb-start))))
        (mwb-msys2-install-base
         (mwb-msy2-x64-get-package-install-fn k))))

(defun mwb-msy2-x64-get-package-install-fn (k)
  (lambda ()
    (mwb-msys2-install-packages (mwb-msys2-get-exec-path) (mwb-msys2-x64-get-path)
                                (mwb-msys2-x64-get-extra-env) mwb-msys2-x64-packages
                                k)))

(defun mwb-msys2-x64-get-libraries-dir ()
  (concat (mwb-msys2-get-current-directory) "mingw64/bin/"))

;; ** x32
(defun mwb-msys2-x32-get-path ()
  (concat "/mingw32/bin:" (mwb-msys2-get-common-path)))

(defun mwb-msys2-x32-get-extra-env ()
  '("MSYSTEM=MINGW32"))

(defun mwb-msys2-x32-ensure (k)
  (if (not (file-exists-p (mwb-msys2-get-current-directory)))
      (mwb-msys2-x32-install k)
    (funcall (mwb-msy2-x32-get-package-install-fn k))))

(defun mwb-msys2-x32-install (&optional k)
  (let ((k (if k k (mwb-start))))
    (mwb-msys2-install-base
     (mwb-msy2-x32-get-package-install-fn k))))

(defun mwb-msy2-x32-get-package-install-fn (k)
  (lambda ()
    (mwb-msys2-install-packages (mwb-msys2-get-exec-path) (mwb-msys2-x32-get-path)
                                (mwb-msys2-x32-get-extra-env) mwb-msys2-x32-packages
                                k)))

(defun mwb-msys2-x32-get-libraries-dir ()
  (concat (mwb-msys2-get-current-directory) "mingw32/bin/"))

;; * Cygwin
(defun mwb-is-cygwin (toolchain)
  (member toolchain '(cygwin-x32 cygwin-x64)))

(defun mwb-cygwin-get-path ()
  (concat ":usr/local/bin:/usr/bin:/bin"))

(defun mwb-cygwin-convert-path (path)
  "Convert path PATH to MinGW format.  c:/Emacs would become /c/Emacs."
  (concat "/cygdrive/" (replace-regexp-in-string ":" "" path)))

(defun mwb-cygwin-install (x &optional k)
  "Install cygwin. If X is 'x32, install 32 bit version."
  (let* ((k (if k k (mwb-start)))
         (install-x32 (if (eq x 'x32) t nil))
         (dir (directory-file-name (if install-x32 mwb-cygwin-x32-directory mwb-cygwin-x64-directory)))
         (installer (if install-x32 mwb-cygwin-x32-dist mwb-cygwin-x64-dist)))
    (mwb-wget-download-file
     installer
     (lambda (installer)
       (let ((process
              (start-file-process-shell-command
               "mwb" (mwb-get-buffer)
               (concat installer " -q -n -B -l \"" mwb-wget-download-directory
                       "\" -s \"" mwb-cygwin-site
                       "\" -R \"" (replace-regexp-in-string "/" "\\\\" dir)
                       "\" -P " (mapconcat 'identity mwb-cygwin-packages ",")))))
         (set-process-sentinel process (mwb-get-cygwin-sentinel k)))))))

(defun mwb-get-cygwin-sentinel(k)
  (lambda (process event)
    (if (not (null mwb-started))
        (when k
          (funcall k))
      (progn
        (message "mwb: Build operation failed. See mwb buffer for more details.")
        (setq mwb-started nil)))))

(defun mwb-cygwin-get-libraries ()
  "Return list of libraries to copy into bin."
  (append mwb-dynamic-libraries mwb-cygwin-dynamic-libraries))

;; ** x64
(defun mwb-cygwin-x64-ensure (k)
  (if (not (file-exists-p mwb-cygwin-x64-directory))
      (mwb-cygwin-install nil k)
    (funcall k)))

(defun mwb-cygwin-x64-get-exec-path ()
  (list (mwb-cygwin-x64-get-libraries-dir)))

(defun mwb-cygwin-x64-get-libraries-dir ()
  (concat (file-name-as-directory mwb-cygwin-x64-directory) "bin"))

;; ** x32
(defun mwb-cygwin-x32-ensure (k)
  (if (not (file-exists-p mwb-cygwin-x32-directory))
      (mwb-cygwin-install 'x32 k)
    (funcall k)))

(defun mwb-cygwin-x32-get-exec-path ()
  (list (mwb-cygwin-x32-get-libraries-dir)))

(defun mwb-cygwin-x32-get-libraries-dir ()
  (concat (file-name-as-directory mwb-cygwin-x32-directory) "bin"))

;; * bsdtar
(defun mwb-extract (file path &optional k)
  "Recursively extracts archives."
  (let ((k (if k k (mwb-start))))
    (mwb-get-bsdtar
     (lambda (bsdtar)
       (when (not (file-exists-p path))
         (mkdir path t))
       (with-current-buffer (mwb-get-buffer)
         (let* ((default-directory path)
                (process
                 (start-file-process-shell-command
                  "mwb" (current-buffer)
                  (concat "\"" bsdtar "\" -xf " file))))
           (set-process-sentinel
            process
            (mwb-get-sentinel
             (lambda () (funcall k))))))))))

(defvar mwb-bsdtar-archives-to-recurse '("tar" "lzma"))

(defun mwb-get-bsdtar (k)
  "Ensure we have bsdtar on our path for unarchiving."
  (let ((bsdtar (mwb-locate-bsdtar)))
    (if bsdtar (funcall k bsdtar)
      (mwb-libarchive-install k))))

(defun mwb-libarchive-install (&optional k)
  (let* ((finished-fn (if k nil (mwb-start)))
         (k (if k k
              (lambda (bsdtar) (funcall finished-fn))))
         (install-dir (mwb-libarchive-get-install-directory)))
    (mwb-get-unzip
     (lambda (unzip)
       (mwb-wget-download-file
        mwb-libarchive-dist
        (lambda (libarchive-dist)
          (let ((inhibit-read-only t))
            (process-file-shell-command
             (concat "\"" unzip "\" -x " libarchive-dist " -d " install-dir) nil "mwb"))
          (funcall k (mwb-locate-bsdtar))))))))

(defun mwb-locate-bsdtar ()
  "Locate bsdtar executable."
  (or (executable-find "bsdtar.exe")
      (locate-file "bsdtar.exe" (mapcar (lambda (path) (concat path "libarchive/bin"))
                                        (mwb-get-paths-from-vars mwb-libarchive-paths)))))

(defun mwb-libarchive-get-install-directory (&optional paths)
  "Find a path where creating a new directory for libarchive is permitted.
If PATHS is not specified start with mwb-libarchive-paths."
  (let ((paths (if paths paths (mwb-get-paths-from-vars mwb-libarchive-paths))))
    (condition-case err
        (let ((directory (concat (car paths)
                                 "libarchive")))
          (when (not (file-exists-p directory))
            (mkdir directory (if (eq directory mwb-wget-download-directory) t nil)))
          directory)
      (error (mwb-libarchive-get-install-directory (cdr paths))))))

(defun mwb-get-unzip (k)
  "Ensure we have unzip on our path for unarchiving and then call continuation K."
  (let ((unzip (mwb-locate-unzip)))
    (if unzip unzip
        (mwb-wget-download-file mwb-unzip-dist k))))

(defun mwb-locate-unzip ()
  "Locate unzip execautable."
  (or (executable-find "unzip.exe")
      (locate-file "unzip.exe" (mwb-get-paths-from-vars mwb-unzip-paths))
      nil))

(defun mwb-get-paths-from-vars (paths)
  "In a list PATHS replace all symbols with their values."
  (mapcar (lambda (path) (if (symbolp path)
                             (symbol-value path)
                           path)) paths))

;; * Wget
(defun mwb-wget-download-file (file k)
  "Download FILE using wget and then call continuation K."
  (with-current-buffer (mwb-get-buffer)
   (let* ((local-file (concat (file-name-as-directory mwb-wget-download-directory)
                              (car (reverse (split-string file "/")))))
          (default-directory mwb-wget-download-directory)
          (wget (mwb-wget-get))
          (check-certificate (if (mwb-wget-check-certificate)
                                 "--no-check-certificate" ""))
          (k (lambda () (funcall k local-file))))
     (if (not (file-exists-p local-file))
         (progn
           (when (not (file-exists-p mwb-wget-download-directory))
             (mkdir mwb-wget-download-directory t))
           (let ((process
                  (start-file-process-shell-command
                   "mwb" (current-buffer)
                   (concat "\"" wget
                           "\" " check-certificate " " file))))
             (set-process-sentinel process (mwb-get-sentinel k))))
       (funcall k)))))

(defun mwb-wget-get ()
  "Ensure we have wget on our path for downloading dependencies.
Wget works nicely, since it's able to get stuff from sourceforge."
  (let ((wget (or (executable-find "wget.exe")
              (locate-file "wget.exe" mwb-wget-paths)
              (error "Wget not found"))))
    wget))

(defun mwb-wget-check-certificate ()
  "Whether to pass --no-check-certificate flag to wget.
Needed for GnuWin version, because it fails for https."
  (let ((wget (mwb-wget-get)))
    (if (string-match "GnuWin32" wget)
        t
      nil)))

(defcustom mwb-wget-paths '("c:/Program Files (x86)/GnuWin32/bin/" "c:/Program Files/GnuWin32/bin/")
  "*Paths to search for wget."
  :group 'mwb)

(provide 'ms-windows-builder)
;;; ms-windows-builder.el ends here

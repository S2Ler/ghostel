;;; ghostel-org-test.el --- Tests for ghostel: Org links -*- lexical-binding: t; -*-

;;; Commentary:

;; The `ghostel:' Org link type: storing, following, and completion.

;;; Code:

(require 'ghostel-test-helpers)
(require 'ghostel-org)

(defmacro ghostel-org-test--with-temp-dir (var &rest body)
  "Run BODY with VAR bound to a fresh temporary directory, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "ghostel-org" t))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,var t)))))

(ert-deftest ghostel-test-org-link-type-registered ()
  "Loading `ghostel-org' registers store, follow, and complete functions."
  (should (eq (org-link-get-parameter "ghostel" :store)
              #'ghostel-org-store-link))
  (should (eq (org-link-get-parameter "ghostel" :follow)
              #'ghostel-org-open))
  (should (eq (org-link-get-parameter "ghostel" :complete)
              #'ghostel-org-complete-link)))

(ert-deftest ghostel-test-org-store-link ()
  "Storing in a ghostel buffer links its directory and buffer name."
  (ghostel-test--with-compile-buffer buf
    (setq default-directory (expand-file-name "src/" "~"))
    (let ((org-store-link-plist nil))
      (should (ghostel-org-store-link))
      (should (equal (plist-get org-store-link-plist :link)
                     (format "ghostel:~/src/::%s" (buffer-name))))
      (should (equal (plist-get org-store-link-plist :type) "ghostel"))
      (should (equal (plist-get org-store-link-plist :description)
                     (buffer-name))))))

(ert-deftest ghostel-test-org-open-named-delegates-to-bookmark ()
  "A DIR::NAME link is followed as a bookmark to NAME in DIR.
NAME may itself contain `::'."
  (ghostel-test--with-compile-buffer buf
    (let ((record nil))
      (cl-letf (((symbol-function 'ghostel-bookmark-handler)
                 (lambda (bmk) (setq record bmk) buf)))
        (with-temp-buffer
          (ghostel-org-open "~/ghostel-org-x::*a::b*" nil)
          (should (eq (current-buffer) buf))))
      (should (equal (bookmark-prop-get record 'location)
                     (expand-file-name "~/ghostel-org-x/")))
      (should (equal (bookmark-prop-get record 'buf-name) "*a::b*")))))

(ert-deftest ghostel-test-org-store-link-outside-ghostel ()
  "Outside `ghostel-mode' the store function declines."
  (with-temp-buffer
    (let ((org-store-link-plist nil))
      (should-not (ghostel-org-store-link))
      (should-not org-store-link-plist))))

(ert-deftest ghostel-test-org-open-reuses-live-buffer ()
  "Following pops to a live terminal already in the linked directory."
  (ghostel-org-test--with-temp-dir dir
    (ghostel-test--with-compile-buffer buf
      (setq default-directory dir
            ghostel-identity '((kind . term)))
      (setq-local ghostel--process (ghostel-test--dummy-process "org-live" nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-create)
                     (lambda (&rest _)
                       (ert-fail "Created a new buffer instead of reusing"))))
            (with-temp-buffer
              (ghostel-org-open (abbreviate-file-name dir) nil)
              (should (eq (current-buffer) buf))))
        (delete-process ghostel--process)))))

(ert-deftest ghostel-test-org-open-skips-dead-buffer ()
  "A terminal without a live process in the directory is not reused."
  (ghostel-org-test--with-temp-dir dir
    (ghostel-test--with-compile-buffer buf
      (setq default-directory dir
            ghostel-identity '((kind . term)))
      (let ((created-in nil))
        (cl-letf (((symbol-function 'ghostel-create)
                   (lambda (&rest _) (setq created-in default-directory) buf)))
          (with-temp-buffer
            (ghostel-org-open dir nil)))
        (should (equal created-in dir))))))

(ert-deftest ghostel-test-org-open-skips-command-buffer ()
  "A live command buffer (compile, exec) in the directory is not reused."
  (ghostel-org-test--with-temp-dir dir
    (ghostel-test--with-compile-buffer buf
      (setq default-directory dir
            ghostel-identity '((kind . compile)))
      (setq-local ghostel--process (ghostel-test--dummy-process "org-cmd" nil))
      (let ((created nil))
        (unwind-protect
            (cl-letf (((symbol-function 'ghostel-create)
                       (lambda (&rest _) (setq created t) buf)))
              (with-temp-buffer
                (ghostel-org-open dir nil)))
          (delete-process ghostel--process))
        (should created)))))

(ert-deftest ghostel-test-org-complete-link ()
  "Completion prefixes the chosen directory with `ghostel:'."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) (expand-file-name "src/" "~"))))
    (should (equal (ghostel-org-complete-link) "ghostel:~/src/"))))

(ert-deftest ghostel-test-org-open-creates-shell ()
  "Following with no matching terminal starts a shell in the directory."
  :tags '(native)
  (ghostel-org-test--with-temp-dir dir
    (let ((ghostel-macos-login-shell nil)
          (ghostel-buffer-name " *ghostel-org-create*")
          (buf nil))
      (unwind-protect
          (progn
            (setq buf (ghostel-org-open dir nil))
            (with-current-buffer buf
              (should (eq major-mode 'ghostel-mode))
              (should (process-live-p ghostel--process))
              (should (file-equal-p default-directory dir))))
        (when buf (ghostel-test--cleanup-exec-buffer buf))))))

(ert-deftest ghostel-test-org-open-named-creates-and-reuses ()
  "Following DIR::NAME twice starts NAME once, as a claimable terminal slot."
  :tags '(native)
  (ghostel-org-test--with-temp-dir dir
    (let ((ghostel-macos-login-shell nil)
          (path (concat dir ":: *ghostel-org-named*"))
          (buf nil))
      (unwind-protect
          (progn
            (setq buf (ghostel-org-open path nil))
            (should (equal (buffer-name buf) " *ghostel-org-named*"))
            (should (equal (buffer-local-value 'ghostel-identity buf)
                           '((kind . term)
                             (name . " *ghostel-org-named*")
                             (instance . 1))))
            (should (file-equal-p (buffer-local-value 'default-directory buf)
                                  dir))
            (with-temp-buffer
              (should (eq (ghostel-org-open path nil) buf))))
        (when buf (ghostel-test--cleanup-exec-buffer buf))))))

(provide 'ghostel-org-test)
;;; ghostel-org-test.el ends here

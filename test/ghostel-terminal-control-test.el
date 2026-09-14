;;; ghostel-terminal-control-test.el --- Public terminal control tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Receipts describe local transport acceptance.  The recorder is an independent
;; child witness for bytes, including unexpected prefixes and implicit Return.

;;; Code:

(require 'ghostel-test-helpers)

(defmacro ghostel-test-tc--with-recorder (spec &rest body)
  "Run BODY with a byte-recorder terminal described by SPEC.
SPEC is (BUFFER PROCESS &optional CANONICAL-NO-ECHO)."
  (declare (indent 1))
  (pcase-let ((`(,buffer ,process . ,options) spec))
    `(ghostel-test--with-exec-buffer
         (,buffer ,process (ghostel-test--python)
                  (append
                   (list "-B" "-u"
                         (expand-file-name
                          "terminal_control_recorder.py"
                          (ghostel-test--fixture-directory)))
                   (when ,(car options) '("--canonical-no-echo"))))
       (ghostel-test--wait-for-text "GHOSTEL_CONTROL_READY" ,process)
       ,@body)))

(defun ghostel-test-tc--reset-screen (rows columns)
  "Prepare an empty primary terminal model with ROWS and COLUMNS."
  (ghostel--set-size ghostel--term rows columns)
  (ghostel-test--redraw ghostel--term)
  ;; Erasing cells alone preserves wrap flags from the resized recorder output.
  (ghostel--write-vt ghostel--term "\ec"))

(defun ghostel-test-tc--utf8 (text)
  "Return the UTF-8 bytes of TEXT."
  (encode-coding-string text 'utf-8-unix))

(defun ghostel-test-tc--receipt (result outcome &optional text)
  "Assert RESULT has OUTCOME and, on acceptance, the byte count of TEXT."
  (should (listp result))
  (should (eq outcome (plist-get result :outcome)))
  (should (= 4 (length result)))
  (if (eq outcome 'accepted-locally)
      (should (equal (length (ghostel-test-tc--utf8 text))
                     (plist-get result :utf8-bytes)))
    (let ((reason (plist-get result :reason)))
      (should (and (symbolp reason) reason))
      (should (<= (length (symbol-name reason)) 64))
      (should (string-match-p "\\`[a-z][a-z0-9_-]*\\'"
                              (symbol-name reason))))))

(defun ghostel-test-tc--received (process expected &optional index canonical)
  "Assert PROCESS received EXPECTED before transaction INDEX's delimiter.
CANONICAL selects the newline delimiter for the canonical no-echo fixture."
  (ghostel-send-string (if canonical "\n" "\C-d"))
  (let* ((bytes (ghostel-test-tc--utf8 expected))
         (marker (format "GHOSTEL_CONTROL_%d:" (or index 0)))
         (actual
          (ghostel-test--wait-until
           (lambda ()
             (let ((text (replace-regexp-in-string
                          "\n" "" (ghostel-test--terminal-text))))
               (when (string-match
                      (concat (regexp-quote marker)
                              "\\([0-9]+\\):\\([0-9a-f]\\{64\\}\\):END")
                      text)
                 (list (string-to-number (match-string 1 text))
                       (match-string 2 text)))))
           process)))
    (should (equal actual (list (length bytes) (secure-hash 'sha256 bytes))))))

(defun ghostel-test-tc--snapshot (result text &rest metadata)
  "Assert RESULT contains TEXT and the expected METADATA plist."
  (should (eq 'ok (plist-get result :outcome)))
  (should (equal text (plist-get result :text)))
  (should (= (length (ghostel-test-tc--utf8 text))
             (plist-get result :utf8-bytes)))
  (should (= (if (string-empty-p text) 0 (1+ (cl-count ?\n text)))
             (plist-get result :logical-lines)))
  (dolist (key '(:screen :includes-scrollback :columns :rows :cursor
                :truncated-before :first-line-partial :input-mode
                :emacs-local-input-omitted :graphics))
    (should (plist-member result key)))
  (dolist (key '(:includes-scrollback :truncated-before :first-line-partial
                :emacs-local-input-omitted))
    (should (memq (plist-get result key) '(nil t))))
  (should (memq (plist-get result :screen) '(primary alternate)))
  (should (eq 'omitted (plist-get result :graphics)))
  (while metadata
    (let ((key (pop metadata))
          (value (pop metadata)))
      (ert-info ((format "snapshot field %S" key))
        (should (equal value (plist-get result key))))))
  result)

(defun ghostel-test-tc--input-calls ()
  "Return the four public input requests used to exercise common guards."
  '((ghostel-terminal-control-type "guard-sentinel")
    (ghostel-terminal-control-paste "guard-sentinel\nsecond")
    (ghostel-terminal-control-key "up" nil)
    (ghostel-terminal-control-key "return" nil)))

(ert-deftest ghostel-test-tc-public-api-version ()
  "The public control contract is explicitly versioned."
  (should (boundp 'ghostel-terminal-control-api-version))
  (should (eql 1 ghostel-terminal-control-api-version)))

(ert-deftest ghostel-test-tc-capability-requires-loaded-native-feature ()
  "Function presence cannot advertise an unloaded native module."
  (let ((original-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (unless (eq feature 'ghostel-module)
                   (funcall original-featurep feature subfeature))))
              ((symbol-function 'ghostel--terminal-control-api-version)
               (lambda () 1))
              ((symbol-function 'ghostel--load-module)
               (lambda (&rest _) (ert-fail "Capability probe loaded a module"))))
      (should-not (ghostel-terminal-control-capabilities)))))

(ert-deftest ghostel-test-tc-capability-rejects-native-version-mismatch ()
  "A loaded module must report the same integer API version."
  (let ((original-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (eq feature 'ghostel-module)
                     t
                   (funcall original-featurep feature subfeature)))))
      (dolist (version '(nil 0 2 "1"))
        (ert-info ((format "native version: %S" version))
          (cl-letf (((symbol-function 'ghostel--terminal-control-api-version)
                     (lambda () version)))
            (should-not (ghostel-terminal-control-capabilities)))))
      (cl-letf (((symbol-function 'ghostel--terminal-control-api-version)
                 (lambda () (error "Native probe failed"))))
        (should-not (ghostel-terminal-control-capabilities))))))

(ert-deftest ghostel-test-tc-capability-matching-module ()
  "The real module advertises snapshot and input receipts independently."
  :tags '(native)
  (should (featurep 'ghostel-module))
  (let ((capabilities (ghostel-terminal-control-capabilities)))
    (should (= 8 (length capabilities)))
    (should (eql 1 (plist-get capabilities :api-version)))
    (should (eql 1 (plist-get capabilities :native-api-version)))
    (should (eq t (plist-get capabilities :snapshot-tail)))
    (should (eq t (plist-get capabilities :input-receipts)))))

(ert-deftest ghostel-test-tc-unmanaged-buffer-refuses-operations ()
  "A plain buffer cannot supply terminal text or receive terminal input."
  (with-temp-buffer
    (insert "private ordinary buffer")
    (should (equal '(:outcome not-executed :reason unavailable)
                   (ghostel-terminal-snapshot-tail 10 1000)))
    (dolist (call (ghostel-test-tc--input-calls))
      (ghostel-test-tc--receipt (apply (car call) (cdr call)) 'not-executed))))

(ert-deftest ghostel-test-tc-snapshot-empty-and-grid-metadata ()
  "Snapshots trim blank cells and rows and report native cursor coordinates."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 5 8)
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) ""
     :screen 'primary :includes-scrollback nil :columns 8 :rows 5
     :cursor '(:column 0 :row 0) :truncated-before nil
     :first-line-partial nil :input-mode 'semi-char
     :emacs-local-input-omitted nil)
    (ghostel--write-vt ghostel--term "first   \r\n\r\n界é  \r\n\r\n")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) "first\n\n界é"
     :cursor '(:column 0 :row 4) :truncated-before nil
     :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-soft-and-hard-line-boundaries ()
  "Soft wraps join while hard boundaries preserve logical lines."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 5 6)
    (ghostel--write-vt ghostel--term "abcdefghi\r\nlast\r\n")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) "abcdefghi\nlast"
     :cursor '(:column 0 :row 3) :truncated-before nil
     :first-line-partial nil)
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1 98304) "last"
     :truncated-before t :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-primary-history-and-newest-lines ()
  "The newest logical lines win over retained primary history."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 3 10)
    (ghostel--write-vt ghostel--term "row0\r\nrow1\r\nrow2\r\nrow3\r\nrow4")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304)
     "row0\nrow1\nrow2\nrow3\nrow4"
     :screen 'primary :includes-scrollback t :cursor '(:column 4 :row 2)
     :truncated-before nil :first-line-partial nil)
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 2 98304) "row3\nrow4"
     :truncated-before t :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-alternate-screen-excludes-primary-history ()
  "Alternate reads contain only the current alternate grid."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 3 20)
    (ghostel--write-vt ghostel--term "PRIMARY_SECRET\r\np1\r\np2\r\np3")
    (ghostel--write-vt ghostel--term "\e[?1049h\e[HALT\r\nview")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) "ALT\nview"
     :screen 'alternate :includes-scrollback nil
     :cursor '(:column 4 :row 1) :truncated-before nil)
    (ghostel--write-vt ghostel--term "\e[?1049l")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) "PRIMARY_SECRET\np1\np2\np3"
     :screen 'primary :includes-scrollback t)))

(ert-deftest ghostel-test-tc-snapshot-valid-unicode-suffix ()
  "A partial first line starts at a UTF-8 scalar boundary."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 4 20)
    (ghostel--write-vt ghostel--term "old\r\nAé🙂Z")
    (dolist (case '((5 "🙂Z") (4 "Z")))
      (ghostel-test-tc--snapshot
       (ghostel-terminal-snapshot-tail 1000 (car case)) (cadr case)
       :truncated-before t :first-line-partial t))))

(ert-deftest ghostel-test-tc-snapshot-byte-limit-smaller-than-one-scalar ()
  "A byte limit that fits no scalar omits text without inventing a partial line."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 2 10)
    (ghostel--write-vt ghostel--term "界")
    (dolist (limit '(1 2))
      (ghostel-test-tc--snapshot
       (ghostel-terminal-snapshot-tail 1 limit) ""
       :truncated-before t :first-line-partial nil))
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1 3) "界"
     :truncated-before nil :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-byte-limit-includes-hard-separators ()
  "LF separators count toward the byte limit without making a line partial."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 5 20)
    (ghostel--write-vt ghostel--term "older\r\néé\r\nok\r\n\r\n")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 7) "éé\nok"
     :truncated-before t :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-history-boundary-counts-as-scrollback ()
  "A retained history LF counts as history after all its text is cut."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 1 10)
    (ghostel--write-vt ghostel--term "AA\r\nB")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 1000) "AA\nB"
     :screen 'primary :includes-scrollback t :columns 10 :rows 1
     :cursor '(:column 1 :row 0) :truncated-before nil :first-line-partial nil)
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 1) "B"
     :includes-scrollback nil :truncated-before t :first-line-partial nil)
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 2) "\nB"
     :includes-scrollback t :truncated-before t :first-line-partial t)))

(ert-deftest ghostel-test-tc-snapshot-trailing-blanks-do-not-spend-line-limit ()
  "Trailing blank rows do not evict actual text from a bounded tail."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 6 20)
    (ghostel--write-vt ghostel--term "one\r\ntwo\r\n\r\n")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 2 98304) "one\ntwo"
     :truncated-before nil :first-line-partial nil)))

(ert-deftest ghostel-test-tc-snapshot-thousand-lines-from-large-scrollback ()
  "The 1,000-line tail reaches the newest end of large retained history."
  :tags '(native)
  (let ((ghostel-max-scrollback (* 32 1024 1024)))
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel-test-tc--reset-screen 24 80)
      (ghostel--write-vt
       ghostel--term
       (mapconcat (lambda (n) (format "row-%05d" n))
                  (number-sequence 0 11999) "\r\n"))
      (ghostel-test-tc--snapshot
       (ghostel-terminal-snapshot-tail 1000 98304)
       (mapconcat (lambda (n) (format "row-%05d" n))
                  (number-sequence 11000 11999) "\n")
       :includes-scrollback t :truncated-before t :first-line-partial nil))))

(ert-deftest ghostel-test-tc-snapshot-ninety-six-kibibyte-partial-line ()
  "A wrapped line larger than 96 KiB returns exactly the bounded suffix."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 24 80)
    (ghostel--write-vt ghostel--term
                       (concat "older\r\n" (make-string (+ 98304 17) ?x)))
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 1000 98304) (make-string 98304 ?x)
     :includes-scrollback t :truncated-before t :first-line-partial t)))

(ert-deftest ghostel-test-tc-snapshot-reads-model-without-ui-effects ()
  "Reading uses current native text and leaves Emacs interaction state alone."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 4 20)
    (ghostel--write-vt ghostel--term "model text")
    (ghostel-test--with-rendered-output
      (erase-buffer)
      (insert (propertize "stale Emacs text" 'face 'bold)))
    (goto-char 3)
    (set-mark 7)
    (setq mark-active t)
    (let ((selected (selected-window))
          (original-text (buffer-string))
          (kill-ring '("private kill ring")))
      (cl-letf (((symbol-function 'ghostel--redraw)
                 (lambda (&rest _) (ert-fail "Snapshot forced redraw")))
                ((symbol-function 'ghostel--redraw-now)
                 (lambda (&rest _) (ert-fail "Snapshot forced redraw")))
                ((symbol-function 'redisplay)
                 (lambda (&rest _) (ert-fail "Snapshot forced redisplay")))
                ((symbol-function 'accept-process-output)
                 (lambda (&rest _) (ert-fail "Snapshot yielded")))
                ((symbol-function 'current-kill)
                 (lambda (&rest _) (ert-fail "Snapshot read the kill ring"))))
        (ghostel-test-tc--snapshot
         (ghostel-terminal-snapshot-tail 10 1000) "model text"
         :cursor '(:column 10 :row 0)))
      (should (eq selected (selected-window)))
      (should (eq buf (current-buffer)))
      (should (= 3 (point)))
      (should (= 7 (mark)))
      (should mark-active)
      (should (equal original-text (buffer-string)))
      (should (equal kill-ring '("private kill ring"))))))

(ert-deftest ghostel-test-tc-snapshot-reports-input-mode-and-omits-line-draft ()
  "Every mode is reported, but an Emacs line draft is never terminal text."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 4 20)
    (ghostel--write-vt ghostel--term "child prompt")
    (ghostel-test--insert-rendered "EMACS_LOCAL_DRAFT_SENTINEL")
    (dolist (mode '(char semi-char emacs copy line))
      (setq-local ghostel--input-mode mode)
      (ghostel-test-tc--snapshot
       (ghostel-terminal-snapshot-tail 10 1000) "child prompt"
       :input-mode mode :emacs-local-input-omitted (eq mode 'line)))))

(ert-deftest ghostel-test-tc-snapshot-preserves-next-incremental-redraw ()
  "A snapshot cannot consume the renderer's pending terminal changes."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (ghostel-test-tc--reset-screen 4 20)
    (ghostel--write-vt ghostel--term "before")
    (ghostel-test--redraw ghostel--term t)
    (should (equal "before"
                   (string-trim-right
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (ghostel--write-vt ghostel--term "\r\e[2Kafter")
    (ghostel-test-tc--snapshot
     (ghostel-terminal-snapshot-tail 10 1000) "after")
    (ghostel-test--redraw ghostel--term)
    (should (equal "after"
                   (string-trim-right
                    (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest ghostel-test-tc-snapshot-error-or-quit-is-bounded ()
  "Native snapshot failures return no terminal text or error payload."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (dolist (condition '(error quit))
      (cl-letf (((symbol-function 'ghostel--terminal-snapshot-tail)
                 (lambda (&rest _)
                   (signal condition '("PRIVATE_SNAPSHOT_SENTINEL")))))
        (should (equal '(:outcome not-executed :reason internal-error)
                       (ghostel-terminal-snapshot-tail 10 1000)))))))

(ert-deftest ghostel-test-tc-type-utf8-without-return ()
  "Type sends scalar text once, including valid Unicode format characters."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (let ((text "ASCII é界🙂 é \u200d\u202e\u2066"))
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-type text) 'accepted-locally text)
        (ghostel-test-tc--received proc text)))))

(ert-deftest ghostel-test-tc-text-limit-is-encoded-bytes ()
  "Both text operations accept the one-byte and 65,536-byte boundaries."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (let* ((text (make-string 32768 ?é))
             (wire (concat "\e[200~" text "\e[201~")))
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-type text) 'accepted-locally text)
        (ghostel-test-tc--received proc text 0)
        (ghostel--write-vt ghostel--term "\e[?2004h")
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste text) 'accepted-locally wire)
        (ghostel-test-tc--received proc wire 1)
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-type "x") 'accepted-locally "x")
        (ghostel-test-tc--received proc "x" 2)
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste "z")
         'accepted-locally "\e[200~z\e[201~")
        (ghostel-test-tc--received proc "\e[200~z\e[201~" 3)))))

(ert-deftest ghostel-test-tc-type-rejects-invalid-text-without-writing ()
  "Type rejects controls, non-scalars, malformed UTF-8, and byte overflow."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (dolist (text (append '(nil 17 "" "tab\ttext" "line\ntext" "carriage\rtext")
                           (mapcar #'string (append (number-sequence 0 31)
                                                   (number-sequence 128 159)
                                                   '(#xd800 #xdfff)))
                           (list (unibyte-string #xc3 #x28)
                                 (make-string 65537 ?x)
                                 (concat (make-string 32768 ?é) "x"))))
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-type text) 'not-executed))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-paste-rejects-invalid-text-without-writing ()
  "Paste allows TAB and LF but rejects CR and other unsafe text."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (dolist (text (append '(nil 17 "")
                           (mapcar #'string
                                   (append (remq 9 (remq 10 (number-sequence 0 31)))
                                           (number-sequence 128 159)
                                           '(#xd800 #xdfff)))
                           (list (unibyte-string #xff)
                                 (make-string 65537 ?x)
                                 (concat (make-string 32768 ?é) "x"))))
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste text) 'not-executed))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-paste-is-strict-bracketed-and-counts-framing ()
  "Paste reports framing bytes and never falls back or appends Return."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (let* ((text "first\t界\nsecond")
             (wire (concat "\e[200~" text "\e[201~")))
        (ghostel--write-vt ghostel--term "\e[?2004l")
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste text) 'not-written)
        (ghostel-test-tc--received proc "" 0)
        (ghostel--write-vt ghostel--term "\e[?2004h")
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste text) 'accepted-locally wire)
        (ghostel-test-tc--received proc wire 1)
        (ghostel--write-vt ghostel--term "\e[?2004l")
        (ghostel-test-tc--receipt
         (ghostel-terminal-control-paste text) 'not-written)
        (ghostel-test-tc--received proc "" 2)))))

(ert-deftest ghostel-test-tc-key-follows-terminal-cursor-mode ()
  "The same semantic arrow uses the terminal's current key encoding."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?1l")
      (ghostel-test-tc--receipt
       (ghostel-terminal-control-key "up" nil) 'accepted-locally "\e[A")
      (ghostel-test-tc--received proc "\e[A" 0)
      (ghostel--write-vt ghostel--term "\e[?1h")
      (ghostel-test-tc--receipt
       (ghostel-terminal-control-key "up" nil) 'accepted-locally "\eOA")
      (ghostel-test-tc--received proc "\eOA" 1))))

(ert-deftest ghostel-test-tc-key-modifiers-and-explicit-return ()
  "Modifiers are semantic and submit's canonical Return writes exactly once."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (let ((cases '(("up" (shift) "\e[1;2A")
                     ("a" (shift) "A")
                     ("z" (shift) "Z")
                     ("0" (shift) ")")
                     ("1" (shift) "!")
                     ("9" (shift) "(")
                     ("-" (shift) "_")
                     ("a" (alt shift) "\eA")
                     ("a" (ctrl shift) "\e[97;6u")
                     ("c" (ctrl) "\C-c")
                     ("x" (alt) "\ex")
                     ("up" (ctrl alt shift) "\e[1;8A")
                     ("up" (shift ctrl alt) "\e[1;8A")
                     ("return" nil "\r"))))
        (cl-loop for (key modifiers wire) in cases
                 for index from 0
                 do (ghostel-test-tc--receipt
                     (ghostel-terminal-control-key key modifiers)
                     'accepted-locally wire)
                 do (ghostel-test-tc--received proc wire index))))))

(ert-deftest ghostel-test-tc-key-obeys-kitty-keyboard-mode ()
  "Control keys and submit's Return follow the negotiated keyboard protocol."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[>1u")
      (ghostel-test-tc--receipt
       (ghostel-terminal-control-key "s" '(ctrl))
       'accepted-locally "\e[115;5u")
      (ghostel-test-tc--received proc "\e[115;5u" 0)
      (let ((cases '(("a" (shift) "A")
                     ("a" (alt shift) "\e[97;4u")
                     ("a" (ctrl shift) "\e[97;6u"))))
        (cl-loop for (key modifiers wire) in cases
                 for index from 1
                 do (ghostel-test-tc--receipt
                     (ghostel-terminal-control-key key modifiers)
                     'accepted-locally wire)
                 do (ghostel-test-tc--received proc wire index)))
      (ghostel--write-vt ghostel--term "\e[>8u")
      (ghostel-test-tc--receipt
       (ghostel-terminal-control-key "return" nil)
       'accepted-locally "\e[13u")
      (ghostel-test-tc--received proc "\e[13u" 4))))

(ert-deftest ghostel-test-tc-key-rejects-malformed-semantic-requests ()
  "Raw escapes, noncanonical names, and improper modifiers write nothing."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (dolist (call `((nil nil) (17 nil) ("" nil) ("Up" nil)
                      ("\e[A" nil) ("two words" nil) ("é" nil)
                      (,(make-string 65 ?a) nil)
                      ("up" (shift shift)) ("up" (meta))
                      ("up" ("ctrl")) ("up" shift)
                      ("up" [ctrl]) ("up" (ctrl . alt))))
        (ghostel-test-tc--receipt
         (apply #'ghostel-terminal-control-key call) 'not-executed))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-key-unsupported-is-proven-unwritten ()
  "A valid but unsupported semantic name cannot become literal input."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (dolist (key (list "ghostel-unknown-key" (make-string 64 ?a)))
        (let ((result (ghostel-terminal-control-key key nil)))
          (ghostel-test-tc--receipt result 'not-written)
          (should (eq 'key-unencodable (plist-get result :reason)))))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-line-mode-and-positive-password-state-refuse ()
  "Every input kind refuses line drafts and positive password state."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (dolist (state '((line nil) (semi-char t)))
        (let ((ghostel--input-mode (car state))
              (ghostel--password-mode-p (cadr state)))
          (dolist (call (ghostel-test-tc--input-calls))
            (ghostel-test-tc--receipt
             (apply (car call) (cdr call)) 'not-executed))))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-canonical-no-echo-refuses-real-pty-input ()
  "A successful local canonical/no-echo probe prevents every input kind."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc t)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (dolist (call (ghostel-test-tc--input-calls))
        (ghostel-test-tc--receipt
         (apply (car call) (cdr call)) 'not-executed))
      (ghostel-test-tc--received proc "" 0 t))))

(ert-deftest ghostel-test-tc-unavailable-or-failed-probe-does-not-deny ()
  "Missing sensitive-state evidence leaves valid input available."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (let ((index 0))
        (dolist (probe (list (lambda (_term) nil)
                             (lambda (_term) (error "Local probe unavailable"))))
          (cl-letf (((symbol-function 'ghostel--pty-password-input-p) probe))
            (ghostel-test-tc--receipt
             (ghostel-terminal-control-type "raw-no-echo")
             'accepted-locally "raw-no-echo"))
          (ghostel-test-tc--received proc "raw-no-echo" index)
          (setq index (1+ index)))))))

(ert-deftest ghostel-test-tc-crossed-process-and-terminal-bindings-refuse ()
  "A live process or terminal borrowed from another buffer is unavailable."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (first first-proc)
      (ghostel-test-tc--with-recorder (second second-proc)
        (let ((other-term ghostel--term))
          (with-current-buffer first
            (dolist (swap '(process terminal))
              (let ((ghostel--process (if (eq swap 'process)
                                          second-proc ghostel--process))
                    (ghostel--term (if (eq swap 'terminal)
                                       other-term ghostel--term)))
                (should (equal '(:outcome not-executed :reason unavailable)
                               (ghostel-terminal-snapshot-tail 10 1000)))
                (dolist (call (ghostel-test-tc--input-calls))
                  (ghostel-test-tc--receipt
                   (apply (car call) (cdr call)) 'not-executed)))))
          (with-current-buffer first
            (ghostel-test-tc--received first-proc ""))
          (ghostel-test-tc--received second-proc ""))))))

(ert-deftest ghostel-test-tc-incompatible-native-module-cannot-perform-input ()
  "A mismatched module cannot become a private-write compatibility path."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (cl-letf (((symbol-function 'ghostel--terminal-control-api-version)
                 (lambda () 0)))
        (should (equal '(:outcome not-executed :reason unavailable)
                       (ghostel-terminal-snapshot-tail 10 1000)))
        (dolist (call (ghostel-test-tc--input-calls))
          (ghostel-test-tc--receipt
           (apply (car call) (cdr call)) 'not-executed)))
      (ghostel-test-tc--received proc ""))))

(ert-deftest ghostel-test-tc-dead-process-and-missing-terminal-refuse ()
  "No snapshot or input survives loss of its process or terminal handle."
  :tags '(native)
  (ghostel-test-tc--with-recorder (buf proc)
    (let ((ghostel--term nil))
      (should (equal '(:outcome not-executed :reason unavailable)
                     (ghostel-terminal-snapshot-tail 10 1000)))
      (dolist (call (ghostel-test-tc--input-calls))
        (ghostel-test-tc--receipt (apply (car call) (cdr call)) 'not-executed)))
    (delete-process proc)
    (should (equal '(:outcome not-executed :reason unavailable)
                   (ghostel-terminal-snapshot-tail 10 1000)))
    (dolist (call (ghostel-test-tc--input-calls))
      (ghostel-test-tc--receipt (apply (car call) (cdr call)) 'not-executed))))

(ert-deftest ghostel-test-tc-stopped-native-pty-does-not-fall-back-to-event-pipe ()
  "A live event pipe cannot substitute for a stopped native terminal transport."
  :tags '(native posix)
  (let ((ghostel-use-native-pty t))
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (let ((writes 0)
            snapshot
            receipts)
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (_process _bytes)
                     (setq writes (1+ writes))
                     nil)))
          (ghostel--kill-native-process ghostel--term)
          (should (process-live-p proc))
          (setq snapshot (ghostel-terminal-snapshot-tail 10 1000)
                receipts
                (mapcar (lambda (call) (apply (car call) (cdr call)))
                        (ghostel-test-tc--input-calls))))
        (should (= 0 writes))
        (should (equal '(:outcome not-executed :reason unavailable) snapshot))
        (dolist (receipt receipts)
          (let ((outcome (plist-get receipt :outcome)))
            (should (memq outcome '(not-executed not-written)))
            (ghostel-test-tc--receipt receipt outcome)))))))

(ert-deftest ghostel-test-tc-emacs-transport-receives-one-complete-transaction ()
  "Each accepted input offers one complete encoding to the Emacs transport."
  :tags '(native posix)
  (let ((ghostel-use-native-pty nil))
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (dolist (case '(((ghostel-terminal-control-type "é界") "é界")
                      ((ghostel-terminal-control-paste "a\nb") "\e[200~a\nb\e[201~")
                      ((ghostel-terminal-control-key "up" (shift)) "\e[1;2A")
                      ((ghostel-terminal-control-key "return" nil) "\r")))
        (let (writes)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (process bytes)
                       (push (list process (encode-coding-string bytes 'binary))
                             writes))))
            (ghostel-test-tc--receipt
             (apply (caar case) (cdar case)) 'accepted-locally (cadr case)))
          (should (equal writes
                         (list (list proc (ghostel-test-tc--utf8 (cadr case)))))))))))

(ert-deftest ghostel-test-tc-emacs-transport-error-or-quit-is-uncertain ()
  "Transport entry cannot turn a signal into a safe-to-retry receipt."
  :tags '(native posix)
  (let ((ghostel-use-native-pty nil))
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (dolist (condition '(error quit))
        (dolist (call (ghostel-test-tc--input-calls))
          (let ((writes 0)
                result)
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process _bytes)
                         (setq writes (1+ writes))
                         (signal condition '("PRIVATE_TRANSPORT_SENTINEL")))))
              (setq result (apply (car call) (cdr call))))
            (ghostel-test-tc--receipt result 'uncertain)
            (should (= writes 1))
            (should-not (string-match-p "PRIVATE_TRANSPORT_SENTINEL"
                                         (prin1-to-string result)))))))))

(ert-deftest ghostel-test-tc-input-preserves-interaction-state-and-does-not-read ()
  "Input preserves point, mark, region, mode, focus, and unrelated buffer text."
  :tags '(native posix)
  (ghostel-test--with-pty-matrix backend
    (ghostel-test-tc--with-recorder (buf proc)
      (ghostel--write-vt ghostel--term "\e[?2004h")
      (ghostel-test--with-rendered-output
        (erase-buffer)
        (insert "local display state"))
      (goto-char 3)
      (set-mark 8)
      (setq mark-active t)
      (setq-local ghostel--input-mode 'emacs)
      (let ((selected (selected-window))
            (before (buffer-string))
            (kill-ring '("DO_NOT_READ"))
            (expected "guard-sentinel\e[200~guard-sentinel\nsecond\e[201~\e[A\r"))
        (cl-letf (((symbol-function 'ghostel-terminal-snapshot-tail)
                   (lambda (&rest _) (ert-fail "Input read the terminal first")))
                  ((symbol-function 'current-kill)
                   (lambda (&rest _) (ert-fail "Input read the kill ring")))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest _) (ert-fail "Input yielded")))
                  ((symbol-function 'ghostel--redraw-now)
                   (lambda (&rest _) (ert-fail "Input forced redraw"))))
          (dolist (call (ghostel-test-tc--input-calls))
            (should (eq 'accepted-locally
                        (plist-get (apply (car call) (cdr call)) :outcome))))
          (should (eq selected (selected-window)))
          (should (eq buf (current-buffer)))
          (should (= 3 (point)))
          (should (= 8 (mark)))
          (should mark-active)
          (should (eq 'emacs ghostel--input-mode))
          (should (equal before (buffer-string)))
          (should (equal kill-ring '("DO_NOT_READ"))))
        (ghostel-test-tc--received proc expected)))))

(provide 'ghostel-terminal-control-test)
;;; ghostel-terminal-control-test.el ends here

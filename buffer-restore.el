;;; buffer-restore.el --- Save and restore frame configurations with buffer states -*- lexical-binding: t; -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This package provides manual session management for Emacs frames.
;; Unlike desktop-save-mode, this gives you explicit control over
;; saving and restoring specific frame configurations with their
;; exact buffer states, including positions in PDFs and EPUBs.
;;
;; Session Commands (single frame):
;;   M-x buffer-restore-save-session   - Save current frame configuration
;;   M-x buffer-restore-load-session   - Restore a saved session
;;   M-x buffer-restore-list-sessions  - View all saved sessions
;;   M-x buffer-restore-delete-session - Delete a saved session
;;
;; Workspace Commands (all frames):
;;   M-x buffer-restore-save-workspace   - Save all frames and windows
;;   M-x buffer-restore-load-workspace   - Restore all frames and windows
;;   M-x buffer-restore-list-workspaces  - View all saved workspaces
;;   M-x buffer-restore-delete-workspace - Delete a saved workspace

;;; Code:

(require 'cl-lib)

(defgroup buffer-restore nil
  "Save and restore frame configurations with buffer states."
  :group 'convenience
  :prefix "buffer-restore-")

(defcustom buffer-restore-directory
  (expand-file-name "buffer-restore-sessions" user-emacs-directory)
  "Directory where session files are stored."
  :type 'directory
  :group 'buffer-restore)

(defvar buffer-restore--sessions-file
  (expand-file-name "sessions.el" buffer-restore-directory)
  "File containing the list of all saved sessions.")

(defvar buffer-restore--session-history nil
  "History list for buffer-restore session commands (shared between save and load).")

;;; Session Data Structure
;; A session is a plist with:
;;   :name - session name (string)
;;   :timestamp - when saved (time)
;;   :frame-config - frame geometry
;;   :windows - list of window configurations

(defun buffer-restore--ensure-directory ()
  "Ensure the session directory exists."
  (unless (file-directory-p buffer-restore-directory)
    (make-directory buffer-restore-directory t)))

(defun buffer-restore--serialize-value (value)
  "Ensure VALUE is serializable, converting if necessary."
  (cond
   ((null value) nil)
   ((numberp value) value)
   ((stringp value) value)
   ((symbolp value) value)
   ((listp value)
    ;; Recursively serialize list elements
    (mapcar #'buffer-restore--serialize-value value))
   ;; Unserializable object - return nil
   (t nil)))

(defun buffer-restore--capture-buffer-state (buffer)
  "Capture the complete state of BUFFER for restoration.
Returns a plist with buffer information."
  (with-current-buffer buffer
    (let ((mode major-mode)
          (file (buffer-file-name))
          (point (point))
          (name (buffer-name)))
      (cond
       ;; PDF buffers (pdf-tools)
       ((eq mode 'pdf-view-mode)
        (let ((page (ignore-errors (pdf-view-current-page)))
              (slice (ignore-errors (pdf-view-current-slice)))
              (scale (when (boundp 'pdf-view-display-size)
                       pdf-view-display-size)))
          (list :type 'pdf
                :file file
                :page page
                :slice (buffer-restore--serialize-value slice)
                :scale (buffer-restore--serialize-value scale))))

       ;; EPUB buffers (nov-mode)
       ((eq mode 'nov-mode)
        (list :type 'epub
              :file file
              :index (when (boundp 'nov-documents-index)
                       nov-documents-index)
              :point point))

       ;; Regular file buffers
       ((and file (file-exists-p file))
        (list :type 'file
              :file file
              :point point
              :line (line-number-at-pos point)
              :column (current-column)))

       ;; Dired buffers
       ((eq mode 'dired-mode)
        (list :type 'dired
              :directory default-directory
              :point point))

       ;; Special/internal buffers we can try to restore
       ((string-match-p "^\\*" name)
        (list :type 'special
              :name name
              :mode mode))

       ;; Skip other buffers
       (t nil)))))

(defun buffer-restore--capture-window (window)
  "Capture the state of WINDOW including its buffer.
Returns a plist with window information."
  (let* ((buffer (window-buffer window))
         (buffer-state (buffer-restore--capture-buffer-state buffer)))
    (when buffer-state
      (list :buffer-state buffer-state
            :width (window-total-width window)
            :height (window-total-height window)
            :edges (list (nth 0 (window-edges window))
                        (nth 1 (window-edges window))
                        (nth 2 (window-edges window))
                        (nth 3 (window-edges window)))
            :hscroll (window-hscroll window)
            :vscroll (window-vscroll window t)
            :start (window-start window)
            :point (window-point window)))))

(defun buffer-restore--capture-window-tree (&optional frame)
  "Capture the complete window tree of FRAME.
Returns a nested structure representing the window layout."
  (let ((frame (or frame (selected-frame))))
    (with-selected-frame frame
      (let ((tree (car (window-tree))))
        (buffer-restore--parse-window-tree tree)))))

(defun buffer-restore--parse-window-tree (tree)
  "Parse a window TREE into our session format.
Note: In Emacs window-tree, the direction flag means:
  nil = horizontal split (side-by-side, splits left/right)
  t   = vertical split (top-to-bottom, splits up/down)
We keep the same convention in our storage."
  (if (windowp tree)
      ;; Leaf node - actual window
      (buffer-restore--capture-window tree)
    ;; Internal node - split
    (let* ((direction (car tree))  ; nil=side-by-side, t=stacked
           (edges (cadr tree))
           (children (cddr tree)))
      (list :type 'split
            ;; Store nil for side-by-side, t for stacked
            :horizontal direction
            :edges (list (nth 0 edges) (nth 1 edges)
                        (nth 2 edges) (nth 3 edges))
            :children (delq nil (mapcar #'buffer-restore--parse-window-tree children))))))

(defun buffer-restore--restore-buffer-state (buffer-state)
  "Restore a buffer from BUFFER-STATE.
Returns the restored buffer or nil if restoration failed."
  (let ((type (plist-get buffer-state :type)))
    (condition-case err
        (pcase type
          ('pdf
           (let ((file (plist-get buffer-state :file)))
             (when (and file (file-exists-p file))
               ;; Just open the file, don't set PDF state yet
               ;; We'll do that after the window is set up
               (find-file-noselect file))))

          ('epub
           (let ((file (plist-get buffer-state :file)))
             (when (and file (file-exists-p file))
               (let ((buf (find-file-noselect file)))
                 (with-current-buffer buf
                   (when (eq major-mode 'nov-mode)
                     ;; Restore EPUB state
                     (let ((index (plist-get buffer-state :index))
                           (point (plist-get buffer-state :point)))
                       (when index
                         (nov-goto-document index))
                       (when point
                         (goto-char (min point (point-max)))))))
                 buf))))

          ('file
           (let ((file (plist-get buffer-state :file)))
             (when (and file (file-exists-p file))
               (let ((buf (find-file-noselect file))
                     (point (plist-get buffer-state :point)))
                 (with-current-buffer buf
                   (when point
                     (goto-char (min point (point-max)))))
                 buf))))

          ('dired
           (let ((directory (plist-get buffer-state :directory)))
             (when (and directory (file-directory-p directory))
               (let ((buf (dired-noselect directory))
                     (point (plist-get buffer-state :point)))
                 (with-current-buffer buf
                   (when point
                     (goto-char (min point (point-max)))))
                 buf))))

          ('special
           (let ((name (plist-get buffer-state :name))
                 (mode (plist-get buffer-state :mode)))
             (cond
              ;; Try to restore org-agenda buffers
              ((and (string-prefix-p "*Org Agenda" name)
                    (fboundp 'org-agenda))
               ;; Call org-agenda to recreate the buffer
               (org-agenda nil "a")
               (get-buffer name))

              ;; For other special buffers, just check if they exist
              (t (get-buffer name)))))

          (_ nil))
      (error
       (message "Failed to restore buffer: %s" (error-message-string err))
       nil))))

(defun buffer-restore--get-child-edges (child)
  "Extract edges from CHILD, whether it's a split or a leaf."
  (or (plist-get child :edges)
      (and (plist-get child :buffer-state)
           (plist-get child :edges))))

(defun buffer-restore--calculate-split-size (children horizontal current-index)
  "Calculate the size for splitting based on CHILDREN dimensions.
HORIZONTAL indicates if this is a horizontal split.
CURRENT-INDEX is which child we're currently restoring."
  (when (and children (> (length children) current-index))
    (let* ((current-child (nth current-index children))
           (current-edges (buffer-restore--get-child-edges current-child))
           (remaining-children (nthcdr (1+ current-index) children)))
      (when current-edges
        (if (null remaining-children)
            ;; Last child - don't specify size, let it take remaining space
            nil
          ;; Calculate size needed for current child
          (let ((size (if horizontal
                         ;; For vertical split (stacked), return height
                         (- (nth 3 current-edges) (nth 1 current-edges))
                       ;; For horizontal split (side-by-side), return width
                       (- (nth 2 current-edges) (nth 0 current-edges)))))
            ;; Ensure minimum size
            (max size (if horizontal
                         window-min-height
                       window-min-width))))))))

(defun buffer-restore--apply-buffer-state-to-window (window buffer-state tree)
  "Apply BUFFER-STATE to WINDOW after the buffer is set.
TREE contains additional window state like point and scroll."
  (let ((type (plist-get buffer-state :type)))
    (with-selected-window window
      (pcase type
        ('pdf
         (when (eq major-mode 'pdf-view-mode)
           (let ((page (plist-get buffer-state :page))
                 (slice (plist-get buffer-state :slice))
                 (scale (plist-get buffer-state :scale)))
             (when page
               (pdf-view-goto-page page))
             (when slice
               (apply #'pdf-view-set-slice slice))
             (when (and scale (boundp 'pdf-view-display-size))
               (setq pdf-view-display-size scale)
               (pdf-view-redisplay))
             ;; Skip hscroll/vscroll restoration for PDFs to avoid rendering artifacts
             ;; Force a complete redisplay and give pdf-tools time to settle
             (redisplay t)
             (sit-for 0.1))))

        ('epub
         (when (eq major-mode 'nov-mode)
           (let ((index (plist-get buffer-state :index))
                 (point (plist-get buffer-state :point)))
             (when index
               (nov-goto-document index))
             (when point
               (goto-char (min point (point-max)))))))

        (_
         ;; For regular files, just set point
         (let ((point (plist-get tree :point)))
           (when point
             (goto-char (min point (point-max))))))))))

(defun buffer-restore--restore-window-tree (tree &optional window)
  "Restore window TREE starting from WINDOW (or selected window)."
  (let ((window (or window (selected-window))))
    (if (plist-get tree :buffer-state)
        ;; Leaf - restore the buffer
        (let* ((buffer-state (plist-get tree :buffer-state))
               (buffer (buffer-restore--restore-buffer-state buffer-state)))
          (when buffer
            (set-window-buffer window buffer)
            ;; Apply buffer-specific state (PDF page, EPUB position, etc.)
            ;; This includes scroll positions for PDFs
            (buffer-restore--apply-buffer-state-to-window window buffer-state tree)
            ;; For non-PDF buffers, set window start
            (unless (eq (plist-get buffer-state :type) 'pdf)
              (set-window-start window (plist-get tree :start) t)
              (set-window-hscroll window (plist-get tree :hscroll)))))
      ;; Split - recursively restore children
      (let* ((horizontal (plist-get tree :horizontal))
             (children (plist-get tree :children))
             (num-children (length children)))
        (when (> num-children 0)
          ;; First, create all the window splits and collect the windows
          (let ((windows (list window)))
            (when (> num-children 1)
              (let ((base-window window))
                (dotimes (i (1- num-children))
                  (let* ((split-size (buffer-restore--calculate-split-size
                                     children horizontal i))
                         (new-window (split-window base-window
                                                   split-size
                                                   (if horizontal 'below 'right))))
                    (push new-window windows)
                    (setq base-window new-window)))))
            ;; Now restore buffers to all windows (in correct order)
            (setq windows (nreverse windows))
            (dotimes (i num-children)
              (let ((child (nth i children))
                    (win (nth i windows)))
                (buffer-restore--restore-window-tree child win)))))))))

;;;###autoload
(defun buffer-restore-save-session (name)
  "Save the current frame configuration as a named session.
NAME is the name to give this session."
  (interactive
   (list (completing-read "Session name: "
                         (buffer-restore--list-session-names)
                         nil nil nil
                         'buffer-restore--session-history)))
  (buffer-restore--ensure-directory)

  (let* ((frame (selected-frame))
         (session (list :name name
                       :timestamp (current-time)
                       :frame-width (frame-width frame)
                       :frame-height (frame-height frame)
                       :frame-pixel-width (frame-pixel-width frame)
                       :frame-pixel-height (frame-pixel-height frame)
                       :window-tree (buffer-restore--capture-window-tree frame)))
         (session-file (expand-file-name (concat name ".el")
                                        buffer-restore-directory)))

    ;; Save session to file with print-level and print-length set
    (with-temp-file session-file
      (let ((print-level nil)
            (print-length nil)
            (print-circle nil))
        (insert ";;; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
        (insert ";; Buffer Restore Session File\n")
        (insert ";; Auto-generated - do not edit manually\n\n")
        (prin1 session (current-buffer))
        (insert "\n")))

    ;; Add to beginning of history so it appears with up arrow
    (setq buffer-restore--session-history
          (cons name (delete name buffer-restore--session-history)))

    (message "Session '%s' saved successfully" name)))

;;;###autoload
(defun buffer-restore-load-session (name)
  "Restore a previously saved session by NAME."
  (interactive
   (list (let* ((sessions (buffer-restore--list-session-names))
                (default (car buffer-restore--session-history)))
           (completing-read
            (if default
                (format "Load session (default %s): " default)
              "Load session: ")
            sessions
            nil t nil
            'buffer-restore--session-history
            default))))

  (let ((session-file (expand-file-name (concat name ".el")
                                       buffer-restore-directory)))
    (unless (file-exists-p session-file)
      (user-error "Session '%s' not found" name))

    (let* ((session (with-temp-buffer
                     (insert-file-contents session-file)
                     (read (current-buffer))))
           (frame (selected-frame))
           (frame-width (plist-get session :frame-width))
           (frame-height (plist-get session :frame-height)))

      ;; Restore frame size if saved
      (when (and frame-width frame-height)
        (set-frame-size frame frame-width frame-height))

      ;; Delete all windows except one
      (delete-other-windows)

      ;; Restore window tree
      (let ((window-tree (plist-get session :window-tree)))
        (buffer-restore--restore-window-tree window-tree))

      ;; Add to beginning of history so it appears with up arrow
      (setq buffer-restore--session-history
            (cons name (delete name buffer-restore--session-history)))

      (message "Session '%s' restored" name))))

(defun buffer-restore--list-session-names ()
  "Return a list of all saved session names."
  (when (file-directory-p buffer-restore-directory)
    (mapcar (lambda (file)
              (file-name-sans-extension (file-name-nondirectory file)))
            (directory-files buffer-restore-directory t "\\.el$"))))

;;;###autoload
(defun buffer-restore-list-sessions ()
  "Display a list of all saved sessions with their timestamps."
  (interactive)
  (let ((sessions (buffer-restore--list-session-names)))
    (if (null sessions)
        (message "No saved sessions found")
      (with-current-buffer (get-buffer-create "*Buffer Restore Sessions*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "Saved Sessions:\n\n")
        (dolist (name sessions)
          (let* ((session-file (expand-file-name (concat name ".el")
                                                buffer-restore-directory))
                 (session (with-temp-buffer
                           (insert-file-contents session-file)
                           (read (current-buffer))))
                 (timestamp (plist-get session :timestamp)))
            (insert (format "  %s\n    Saved: %s\n\n"
                          name
                          (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))))
        (goto-char (point-min))
        (read-only-mode 1)
        (display-buffer (current-buffer))))))

;;;###autoload
(defun buffer-restore-delete-session (name)
  "Delete a saved session by NAME."
  (interactive
   (list (completing-read "Delete session: "
                         (buffer-restore--list-session-names)
                         nil t)))

  (let ((session-file (expand-file-name (concat name ".el")
                                       buffer-restore-directory)))
    (if (file-exists-p session-file)
        (when (yes-or-no-p (format "Delete session '%s'? " name))
          (delete-file session-file)
          (message "Session '%s' deleted" name))
      (user-error "Session '%s' not found" name))))

;;;###autoload
(defun buffer-restore-debug-current-state ()
  "Debug function to show what would be captured for current frame.
Displays the session data structure in a readable format."
  (interactive)
  (let* ((window-tree (buffer-restore--capture-window-tree))
         (session (list :name "debug"
                       :timestamp (current-time)
                       :window-tree window-tree)))
    (with-current-buffer (get-buffer-create "*Buffer Restore Debug*")
      (erase-buffer)
      (emacs-lisp-mode)
      (insert (format ";; Window tree captured at %s\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))
      (let ((print-level nil)
            (print-length nil))
        (pp session (current-buffer)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun buffer-restore-debug-workspace ()
  "Debug function to show what would be captured for all frames.
Displays the workspace data structure in a readable format."
  (interactive)
  (let* ((frames (frame-list))
         (frame-sessions
          (mapcar (lambda (frame)
                    (let* ((native-pos (frame-position frame))
                           (pos-left (car native-pos))
                           (pos-top (cdr native-pos))
                           ;; Get both inner and outer edges for comparison
                           (outer-edges (frame-edges frame 'outer-edges))
                           (inner-edges (frame-edges frame 'inner-edges))
                           ;; Calculate offset from outer to inner (decorations)
                           (left-offset (- (nth 0 inner-edges) (nth 0 outer-edges)))
                           (top-offset (- (nth 1 inner-edges) (nth 1 outer-edges)))
                           ;; Use native position and add decoration offset
                           (actual-left (+ pos-left left-offset))
                           (actual-top (+ pos-top top-offset)))
                      (list :frame-width (frame-width frame)
                            :frame-height (frame-height frame)
                            :frame-pixel-width (frame-pixel-width frame)
                            :frame-pixel-height (frame-pixel-height frame)
                            :frame-left actual-left
                            :frame-top actual-top
                            :debug-native-pos (cons pos-left pos-top)
                            :debug-outer-edges outer-edges
                            :debug-inner-edges inner-edges
                            :window-tree (buffer-restore--capture-window-tree frame))))
                  frames))
         (workspace (list :name "debug-workspace"
                         :timestamp (current-time)
                         :frames frame-sessions)))
    (with-current-buffer (get-buffer-create "*Buffer Restore Workspace Debug*")
      (erase-buffer)
      (emacs-lisp-mode)
      (insert (format ";; Workspace captured at %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format ";; Total frames: %d\n\n" (length frames)))
      (let ((print-level nil)
            (print-length nil))
        (pp workspace (current-buffer)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Workspace Functions (All Frames)

(defvar buffer-restore--workspace-directory
  (expand-file-name "buffer-restore-workspaces" user-emacs-directory)
  "Directory where workspace files are stored.")

(defvar buffer-restore--workspace-history nil
  "History list for buffer-restore workspace commands (shared between save and load).")

(defun buffer-restore--ensure-workspace-directory ()
  "Ensure the workspace directory exists."
  (unless (file-directory-p buffer-restore--workspace-directory)
    (make-directory buffer-restore--workspace-directory t)))

(defun buffer-restore--list-workspace-names ()
  "Return a list of all saved workspace names."
  (when (file-directory-p buffer-restore--workspace-directory)
    (mapcar (lambda (file)
              (file-name-sans-extension (file-name-nondirectory file)))
            (directory-files buffer-restore--workspace-directory t "\\.el$"))))

;;;###autoload
(defun buffer-restore-save-workspace (name)
  "Save all frames and windows as a named workspace.
NAME is the name to give this workspace."
  (interactive
   (list (completing-read "Workspace name: "
                         (buffer-restore--list-workspace-names)
                         nil nil nil
                         'buffer-restore--workspace-history)))
  (buffer-restore--ensure-workspace-directory)

  (let* ((selected-frame-obj (selected-frame))
         (frames (frame-list))
         (frame-sessions
          (mapcar (lambda (frame)
                    (let* ((native-pos (frame-position frame))
                           (pos-left (car native-pos))
                           (pos-top (cdr native-pos))
                           ;; Get both inner and outer edges for comparison
                           (outer-edges (frame-edges frame 'outer-edges))
                           (inner-edges (frame-edges frame 'inner-edges))
                           ;; Calculate offset from outer to inner (decorations)
                           (left-offset (- (nth 0 inner-edges) (nth 0 outer-edges)))
                           (top-offset (- (nth 1 inner-edges) (nth 1 outer-edges)))
                           ;; Use native position and add decoration offset
                           (actual-left (+ pos-left left-offset))
                           (actual-top (+ pos-top top-offset))
                           ;; Mark if this is the selected/dominant frame
                           (is-selected (eq frame selected-frame-obj)))
                      (list :frame-width (frame-width frame)
                            :frame-height (frame-height frame)
                            :frame-pixel-width (frame-pixel-width frame)
                            :frame-pixel-height (frame-pixel-height frame)
                            ;; Store actual pixel positions
                            :frame-left actual-left
                            :frame-top actual-top
                            :dominant is-selected
                            :window-tree (buffer-restore--capture-window-tree frame))))
                  frames))
         (workspace (list :name name
                         :timestamp (current-time)
                         :frames frame-sessions))
         (workspace-file (expand-file-name (concat name ".el")
                                          buffer-restore--workspace-directory)))

    ;; Save workspace to file
    (with-temp-file workspace-file
      (let ((print-level nil)
            (print-length nil)
            (print-circle nil))
        (insert ";;; -*- mode: emacs-lisp; coding: utf-8 -*-\n")
        (insert ";; Buffer Restore Workspace File\n")
        (insert ";; Auto-generated - do not edit manually\n\n")
        (prin1 workspace (current-buffer))
        (insert "\n")))

    ;; Add to beginning of history so it appears with up arrow
    (setq buffer-restore--workspace-history
          (cons name (delete name buffer-restore--workspace-history)))

    ;; Show which frames were saved with useful identifiers
    (let ((msg (format "Workspace '%s' saved successfully (%d frames: %s)"
                       name
                       (length frames)
                       (mapconcat
                        (lambda (f)
                          ;; Get the first visible window's buffer that has a file
                          (let* ((windows (window-list f 'no-minibuf))
                                 (file-buffer
                                  (cl-find-if
                                   (lambda (w)
                                     (buffer-file-name (window-buffer w)))
                                   windows)))
                            (if file-buffer
                                (file-name-nondirectory
                                 (buffer-file-name (window-buffer file-buffer)))
                              ;; Fallback to frame position if no file found
                              (format "frame@%d,%d"
                                      (car (frame-position f))
                                      (cdr (frame-position f))))))
                        frames ", "))))
      (message "%s" msg)
      ;; Clear the message after 2 seconds
      (run-at-time 2 nil (lambda () (message nil))))))

;;;###autoload
(defun buffer-restore-load-workspace (name)
  "Restore a previously saved workspace by NAME.
This will close all existing frames and restore the saved workspace."
  (interactive
   (list (let* ((workspaces (buffer-restore--list-workspace-names))
                (default (car buffer-restore--workspace-history)))
           (completing-read
            (if default
                (format "Load workspace (default %s): " default)
              "Load workspace: ")
            workspaces
            nil t nil
            'buffer-restore--workspace-history
            default))))

  (let ((workspace-file (expand-file-name (concat name ".el")
                                         buffer-restore--workspace-directory)))
    (unless (file-exists-p workspace-file)
      (user-error "Workspace '%s' not found" name))

    (let* ((workspace (with-temp-buffer
                       (insert-file-contents workspace-file)
                       (read (current-buffer))))
           (frame-sessions (plist-get workspace :frames))
           ;; Find which session is marked as dominant
           (dominant-index (cl-position-if (lambda (s) (plist-get s :dominant)) frame-sessions))
           ;; Split sessions into dominant and non-dominant
           (dominant-session (when dominant-index (nth dominant-index frame-sessions)))
           (other-sessions (when dominant-index
                            (append (cl-subseq frame-sessions 0 dominant-index)
                                   (when (< (1+ dominant-index) (length frame-sessions))
                                     (cl-subseq frame-sessions (1+ dominant-index)))))))

      ;; Close all frames except one
      (let ((frames-to-delete (cdr (frame-list))))
        (dolist (frame frames-to-delete)
          (delete-frame frame)))

      ;; Kill all file-visiting buffers to ensure clean restoration
      (dolist (buffer (buffer-list))
        (when (or (buffer-file-name buffer)
                  (with-current-buffer buffer
                    (memq major-mode '(pdf-view-mode nov-mode))))
          (kill-buffer buffer)))

      ;; Restore dominant frame first in the existing frame (or first if no dominant)
      (when (> (length frame-sessions) 0)
        (let* ((first-session (or dominant-session (car frame-sessions)))
               (frame (selected-frame))
               (frame-width (plist-get first-session :frame-width))
               (frame-height (plist-get first-session :frame-height))
               (frame-left (plist-get first-session :frame-left))
               (frame-top (plist-get first-session :frame-top)))

          ;; Restore frame position first (before size changes)
          (when (and frame-left frame-top (numberp frame-left) (numberp frame-top))
            (set-frame-position frame frame-left frame-top))

          ;; Then restore frame size
          (when (and frame-width frame-height)
            (set-frame-size frame frame-width frame-height))

          ;; Delete all windows and restore window tree
          (delete-other-windows)
          (let ((window-tree (plist-get first-session :window-tree)))
            (buffer-restore--restore-window-tree window-tree))

          ;; Raise frame to ensure it's visible
          (raise-frame frame)))

      ;; Create new frames for remaining (non-dominant) sessions
      (let ((frame-num 1)
            (remaining-sessions (if dominant-session
                                   other-sessions
                                 (cdr frame-sessions))))
        (dolist (session remaining-sessions)
          (setq frame-num (1+ frame-num))
          (let* ((frame-width (plist-get session :frame-width))
                 (frame-height (plist-get session :frame-height))
                 (frame-left (plist-get session :frame-left))
                 (frame-top (plist-get session :frame-top)))

            ;; Create frame with position parameters
            (let ((new-frame (make-frame
                             (append
                              (when (and frame-left frame-top
                                        (numberp frame-left) (numberp frame-top))
                                (list (cons 'left frame-left)
                                      (cons 'top frame-top)))
                              (when (and frame-width frame-height)
                                (list (cons 'width frame-width)
                                      (cons 'height frame-height)))))))

              ;; Verify and adjust position if needed
              (when (and frame-left frame-top
                        (numberp frame-left) (numberp frame-top))
                (set-frame-position new-frame frame-left frame-top))

              ;; Restore window tree in new frame
              (with-selected-frame new-frame
                (delete-other-windows)
                (let ((window-tree (plist-get session :window-tree)))
                  (buffer-restore--restore-window-tree window-tree)))

              ;; Ensure frame is visible (don't use lower-frame as it may iconify)
              (make-frame-visible new-frame)))))

      ;; Finally, raise and focus the dominant frame to ensure it's on top of everything
      (let ((dominant-frame (selected-frame)))
        ;; Multiple attempts to grab focus and raise above all applications
        (raise-frame dominant-frame)
        (when (fboundp 'x-focus-frame)
          (x-focus-frame dominant-frame))
        (select-frame-set-input-focus dominant-frame)
        ;; Force window manager attention by briefly setting always-on-top
        (set-frame-parameter dominant-frame 'z-group 'above)
        (redisplay t)
        (set-frame-parameter dominant-frame 'z-group nil))

      ;; Add to beginning of history so it appears with up arrow
      (setq buffer-restore--workspace-history
            (cons name (delete name buffer-restore--workspace-history)))

      (message "Workspace '%s' restored (%d frames)" name (length frame-sessions)))))

;;;###autoload
(defun buffer-restore-list-workspaces ()
  "Display a list of all saved workspaces with their timestamps."
  (interactive)
  (let ((workspaces (buffer-restore--list-workspace-names)))
    (if (null workspaces)
        (message "No saved workspaces found")
      (with-current-buffer (get-buffer-create "*Buffer Restore Workspaces*")
        (read-only-mode -1)
        (erase-buffer)
        (insert "Saved Workspaces:\n\n")
        (dolist (name workspaces)
          (let* ((workspace-file (expand-file-name (concat name ".el")
                                                  buffer-restore--workspace-directory))
                 (workspace (with-temp-buffer
                             (insert-file-contents workspace-file)
                             (read (current-buffer))))
                 (timestamp (plist-get workspace :timestamp))
                 (num-frames (length (plist-get workspace :frames))))
            (insert (format "  %s\n    Saved: %s (%d frames)\n\n"
                          name
                          (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                          num-frames))))
        (goto-char (point-min))
        (read-only-mode 1)
        (display-buffer (current-buffer))))))

;;;###autoload
(defun buffer-restore-delete-workspace (name)
  "Delete a saved workspace by NAME."
  (interactive
   (list (completing-read "Delete workspace: "
                         (buffer-restore--list-workspace-names)
                         nil t)))

  (let ((workspace-file (expand-file-name (concat name ".el")
                                         buffer-restore--workspace-directory)))
    (if (file-exists-p workspace-file)
        (when (yes-or-no-p (format "Delete workspace '%s'? " name))
          (delete-file workspace-file)
          (message "Workspace '%s' deleted" name))
      (user-error "Workspace '%s' not found" name))))

(provide 'buffer-restore)
;;; buffer-restore.el ends here

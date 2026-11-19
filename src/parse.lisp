(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; Parsers of git's NUL-separated machine output. Pure functions of a
;;; string (the UTF-8 decoded stdout); no git runs. Paths come back exactly
;;; as git printed them (with -z git never quotes). Malformed input signals
;;; REVISION-PROTOCOL:REVISION-ERROR whose message names the offending
;;; record. Each returns a fresh list of plists in input order; tests compare
;;; by key (GETF), so key order inside a plist is not part of the contract.
;;; ------------------------------------------------------------------

(defun %malformed (what record)
  (error 'rev:revision-error
         :message (format nil "malformed ~a record ~s" what record)))

(defun %nul-records (input)
  "INPUT split at NUL characters; the empty record after a final NUL dropped."
  (check-type input string)
  (if (zerop (length input))
      '()
      (let ((parts (%split-string input (code-char 0))))
        (if (string= "" (car (last parts)))
            (butlast parts)
            parts))))

(defun %fields (record n)
  "The first N space-separated tokens of RECORD and the rest (a non-empty
string) → (values tokens rest), or NIL when RECORD is too short."
  (let ((tokens '()) (start 0))
    (dotimes (i n)
      (let ((sp (position #\Space record :start start)))
        (unless sp (return-from %fields nil))
        (push (subseq record start sp) tokens)
        (setf start (1+ sp))))
    (when (< start (length record))
      (values (nreverse tokens) (subseq record start)))))

(defun %octal-field (what record s)
  (if (and (plusp (length s)) (every (lambda (c) (digit-char-p c 8)) s))
      (parse-integer s :radix 8)
      (%malformed what record)))

(defun %digits (what record s)
  (if (and (plusp (length s)) (every #'digit-char-p s))
      (parse-integer s)
      (%malformed what record)))

(defun parse-status-porcelain-v2 (input)
  "Parse `git status --porcelain=v2 -z [--branch]` output INPUT (a string).
Header records (\"# …\") are skipped. → list of plists, one per entry:

  \"1 XY sub mH mI mW hH hI path\"
     (:kind :changed :xy \"XY\" :submodule sub :mode-head mH :mode-index mI
      :mode-worktree mW :id-head hH :id-index hI :path path)
  \"2 XY sub mH mI mW hH hI Xscore path\" NUL \"origPath\"
     the same keys with :kind :renamed (X = R) or :copied (X = C),
     plus :score (integer) and :orig-path origPath
  \"u XY sub m1 m2 m3 mW h1 h2 h3 path\"
     (:kind :unmerged :xy \"XY\" :submodule sub :modes (m1 m2 m3 mW)
      :ids (h1 h2 h3) :path path)
  \"? path\"  (:kind :untracked :path path)
  \"! path\"  (:kind :ignored :path path)

XY and sub are strings as printed (\".M\", \"N...\"); modes are integers
read in octal; ids are strings. \"\" → NIL."
  (let ((records (%nul-records input))
        (out '())
        (what "status --porcelain=v2"))
    (loop while records
          do (let* ((rec (pop records))
                    (head (if (plusp (length rec)) (char rec 0) #\Nul)))
               (flet ((octal (s) (%octal-field what rec s)))
                 (cond
                   ((char= head #\#))
                   ((and (member head '(#\? #\!)) (> (length rec) 2)
                         (char= #\Space (char rec 1)))
                    (push (list :kind (if (char= head #\?) :untracked :ignored)
                                :path (subseq rec 2))
                          out))
                   ((char= head #\1)
                    (multiple-value-bind (f path) (%fields rec 8)
                      (unless (and f (string= "1" (first f))) (%malformed what rec))
                      (destructuring-bind (one xy sub mh mi mw hh hi) f
                        (declare (ignore one))
                        (push (list :kind :changed :xy xy :submodule sub
                                    :mode-head (octal mh) :mode-index (octal mi)
                                    :mode-worktree (octal mw) :id-head hh :id-index hi
                                    :path path)
                              out))))
                   ((char= head #\2)
                    (multiple-value-bind (f path) (%fields rec 9)
                      (unless (and f (string= "2" (first f)) records) (%malformed what rec))
                      (destructuring-bind (two xy sub mh mi mw hh hi score) f
                        (declare (ignore two))
                        (let ((letter (and (plusp (length score)) (char score 0))))
                          (unless (member letter '(#\R #\C)) (%malformed what rec))
                          (push (list :kind (if (char= letter #\R) :renamed :copied)
                                      :xy xy :submodule sub
                                      :mode-head (octal mh) :mode-index (octal mi)
                                      :mode-worktree (octal mw) :id-head hh :id-index hi
                                      :score (%digits what rec (subseq score 1))
                                      :path path :orig-path (pop records))
                                out)))))
                   ((char= head #\u)
                    (multiple-value-bind (f path) (%fields rec 10)
                      (unless (and f (string= "u" (first f))) (%malformed what rec))
                      (destructuring-bind (u xy sub m1 m2 m3 mw h1 h2 h3) f
                        (declare (ignore u))
                        (push (list :kind :unmerged :xy xy :submodule sub
                                    :modes (mapcar #'octal (list m1 m2 m3 mw))
                                    :ids (list h1 h2 h3) :path path)
                              out))))
                   (t (%malformed what rec))))))
    (nreverse out)))

(defun parse-name-status (input)
  "Parse `git diff-tree -r -z -M --name-status --no-commit-id` (or `git diff
--name-status -z -M`) output INPUT (a string). → list of plists:

  \"A\" NUL path           (:status :add :path path)
  \"D\" NUL path           (:status :remove :path path)
  \"M\" NUL path           (:status :modify :path path)
  \"T\" NUL path           (:status :type-change :path path)
  \"U\" NUL path           (:status :unmerged :path path)
  \"Rnnn\" NUL old NUL new (:status :move :score nnn :from old :path new)
  \"Cnnn\" NUL old NUL new (:status :copy :score nnn :from old :path new)

A status letter followed by digits elsewhere (\"M100\") keeps its kind and
adds :score. Any other letter → REVISION-ERROR. \"\" → NIL."
  (let ((records (%nul-records input))
        (out '())
        (what "name-status"))
    (loop while records
          do (let* ((rec (pop records))
                    (kind (and (plusp (length rec))
                               (cdr (assoc (char rec 0)
                                           '((#\A . :add) (#\D . :remove) (#\M . :modify)
                                             (#\T . :type-change) (#\U . :unmerged)
                                             (#\R . :move) (#\C . :copy))))))
                    (digits (and kind (subseq rec 1))))
               (unless (and kind (every #'digit-char-p digits)) (%malformed what rec))
               (let ((score (and (plusp (length digits)) (parse-integer digits))))
                 (if (member kind '(:move :copy))
                     (let ((from (pop records)) (to (pop records)))
                       (unless (and from to) (%malformed what rec))
                       (push (list* :status kind :from from :path to
                                    (and score (list :score score)))
                             out))
                     (let ((path (pop records)))
                       (unless path (%malformed what rec))
                       (push (list* :status kind :path path (and score (list :score score)))
                             out))))))
    (nreverse out)))

(defun parse-ls-tree (input)
  "Parse `git ls-tree -z [-l] [-r] [-t]` output INPUT (a string). Each
record is \"mode SP type SP id[ SP+ size] TAB path\". → list of plists
(:mode m :type :blob|:tree|:commit :id id :size n :path path), MODE an integer
read in octal, SIZE an integer for blobs listed with -l, else NIL (git prints
\"-\" for trees and submodules). \"\" → NIL."
  (let ((what "ls-tree"))
    (loop for rec in (%nul-records input)
          collect (let ((tab (position #\Tab rec)))
                    (unless (and tab (< (1+ tab) (length rec))) (%malformed what rec))
                    (let ((meta (remove "" (%split-string (subseq rec 0 tab) #\Space)
                                        :test #'string=)))
                      (unless (<= 3 (length meta) 4) (%malformed what rec))
                      (destructuring-bind (mode type id &optional size) meta
                        (list :mode (%octal-field what rec mode)
                              :type (cond ((string= type "blob") :blob)
                                          ((string= type "tree") :tree)
                                          ((string= type "commit") :commit)
                                          (t (%malformed what rec)))
                              :id id
                              :size (if (or (null size) (string= size "-"))
                                        nil
                                        (%digits what rec size))
                              :path (subseq rec (1+ tab)))))))))

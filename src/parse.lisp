(in-package #:revision-backend-git)

;;; ------------------------------------------------------------------
;;; Parsers of git's NUL-separated machine output. Pure functions of a
;;; string (the UTF-8 decoded stdout); no git runs. Paths come back exactly
;;; as git printed them (with -z git never quotes). Malformed input signals
;;; REVISION-PROTOCOL:REVISION-ERROR whose message names the offending
;;; record. Each returns a fresh list of plists in input order; tests compare
;;; by key (GETF), so key order inside a plist is not part of the contract.
;;; ------------------------------------------------------------------

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
  (declare (ignore input))
  (%not-implemented 'parse-status-porcelain-v2))

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
  (declare (ignore input))
  (%not-implemented 'parse-name-status))

(defun parse-ls-tree (input)
  "Parse `git ls-tree -z [-l] [-r] [-t]` output INPUT (a string). Each
record is \"mode SP type SP id[ SP+ size] TAB path\". → list of plists
(:mode m :type :blob|:tree|:commit :id id :size n :path path), MODE an integer
read in octal, SIZE an integer for blobs listed with -l, else NIL (git prints
\"-\" for trees and submodules). \"\" → NIL."
  (declare (ignore input))
  (%not-implemented 'parse-ls-tree))

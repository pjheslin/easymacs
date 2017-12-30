;;; oed-org.el --- A package to interrogate the OED's API for the definition of the word at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Thomas Worthington

;; Author: Thomas Worthington <thomas.worthington@london.ac.uk>
;; Keywords: convenience

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
;; Code to look up the word at point in the OED on-line API and
;; display it in an org-mode temp buffer.
;;

;;; Code:

(require 'request)
(require 'json)

;;; Customisation
(defgroup oed-org nil
  "API credentials for accessing the OED online"
  :group 'convenience)

(defcustom oed-app-id "30dc10a3"
  "App ID obtained from OED"
  :type 'string
  :group 'oed-org)

(defcustom oed-app-key "7353ef66b8ad8e4074e5295a765dcc21"
  "App key obtained from OED"
  :type 'string
  :group 'oed-org)

(defvar oed-cache nil
  "A place for the request calls to store their result for later processing by various functions.")

(defun unescape-string(s)
  (decode-coding-string s 'utf-8))

(defun oed-lookup (url)
  "Fetch data from the given URL and set oed-cache to the .results part thereof. Hide errors."
  (interactive "s")
  (setq oed-cache nil)
  (request url
   :headers `(("app_id" . ,oed-app-id)
              ("app_key" . ,oed-app-key)
              ("Accept" . "application/json"))
   :parser 'json-read
   :sync t
   :error (cl-function ((lambda (&key data &allow-other-keys) )))
   :timeout 10
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (let-alist data
                             (setq oed-cache .results)
                             )
                           ))))

(defun oed-lookup-word (word)
  "Lookup WORD in the entries section."
  (interactive "s")
  (oed-lookup
   (concat "https://od-api.oxforddictionaries.com:443/api/v1/entries/en/" (url-encode-url word)))
)

(defun oed-lookup-lemmatron (word)
  "Find the root WORD of an inflection."
  (interactive "s")
  (oed-lookup
   (concat "https://od-api.oxforddictionaries.com:443/api/v1/inflections/en/" (url-encode-url word))))

(defun oed-lookup-synonyms (word)
  "Lookup WORD in the entries section."
  (interactive "s")
  (oed-lookup
   (concat "https://od-api.oxforddictionaries.com:443/api/v1/entries/en/" (url-encode-url word) "/synonyms;antonyms"))
)

(defun oed-jpath (data path)
  "Access element of DATA using a PATH of the form (index index....) where an index is either an array index or a string."
  (let ((dp (copy-tree data))
        (idx 0))
    (when dp
      (dolist (idx path dp)
        (setq idx (pop path))
        (if (numberp idx)
            (setq dp (aref dp idx))
          (setq dp (cdr (assoc idx dp)))
          )
        )
      )
    )
  )

(defun oed-lemma (word)
  "Return the root word for the given word"
  (oed-lookup-lemmatron word)
  (let ((result (oed-jpath oed-cache '(0 lexicalEntries 0))))
    (if (assoc 'inflectionOf result)
        (oed-jpath result '(inflectionOf 0 text))
      (oed-jpath result '(derivativeOf 0 text))
  )))

(defun oed-cprint (&rest things)
  "Concatenate and print things"
  (mapc 'princ things)
  (terpri nil t))

(defun oed-clprint (&rest things)
  "Concatenate and print things without newline"
  (mapc 'princ things))


(defun oed-printcdrs(things &optional label pre post)
  "Print the cdrs of things (which is assumed to be an associative list) one per line labeled with label"
  (or label (setq label ""))
  (or pre (setq pre ""))
  (mapc (lambda (x)
          (oed-cprint   (concat label (oed-wrap (decode-coding-string (cdr x) 'utf-8) pre post)))) things )
  )

(defun oed-vhead (vector)
  "Return the first item in an array"
  (if (< 0 (length vector))
      (aref vector 0)
    nil))

(defun oed-word ()
  "Return the keyword currently held in the cache."
  (unescape-string (alist-get 'word (oed-vhead oed-cache))))

(defun oed-wrap(thing pre &optional post)
  (or post (setq post pre))
  (concat pre thing post))

(defun oed-expand-examples(raw depth)
  "Present any examples (RAW) as italicised text indented to DEPTH."
  (let ((examples raw)
        (extext))
    (mapc (lambda(e)(let-alist e
                       (setq extext (concat "/" .text "/"))
                       (if .registers
                           (setq extext (concat "[" (string-join (append .registers nil) ",") "] " extext)))
                       (oed-cprint (make-string depth ? ) "- " (unescape-string extext)))) examples)))

(defun oed-expand-sense (raw &optional depth)
  "Print the definition of a sense sub-tree (RAW), the examples associated with it, and recursively expand any sub-senses. Add indentation and stars appropriate to an org entry of DEPTH."
  (or depth (setq depth 2))
  (let-alist raw
;    (setq .definitions (or .definitions .crossReferenceMarkers ))
    (when .definitions
      (oed-cprint
       "\n"
       (make-string depth ?*)
       " "
       (if (or .regions .domains) (concat "[" (string-join (append .domains (append .regions nil)) ", " ) "] ") "" )
       (unescape-string (oed-vhead .definitions))
       (if .registers (concat " [" (string-join (append .registers nil) ", " ) "] ") "" )
       )
      )
    (when .notes
      (mapc (lambda (n) (oed-cprint (make-string depth ? ) "*Note:* " (unescape-string(alist-get 'text n)))) .notes))
    (if .variantForms
        (oed-cprint "  - Also: " (unescape-string (oed-listcollect .variantForms  'text))))
    (when (and .crossReferenceMarkers (not .definitions))
      (mapc (lambda (n) (oed-cprint (make-string depth ? ) "\n - " (unescape-string n))) .crossReferenceMarkers))
    (when .examples
      (oed-cprint "")
      (oed-expand-examples .examples depth)
      )
    (when .subsenses
      (oed-cprint "")
      (mapc (lambda(s)(oed-expand-sense s (1+ depth))) .subsenses)
      ))
  )

(defun oed-expand-synonym (raw &optional depth)
    "Search a sense sub-tree (RAW) for antonyms, the examples associated with it, and recursively expand any sub-senses, outputing as we go.
Add indentation and stars appropriate to an org entry of DEPTH."
  (or depth (setq depth 2))
  (let-alist raw
    (when .synonyms
      (if .examples
            (oed-clprint "\n"
                         (make-string depth ?*)
                         (unescape-string (concat " /"
                                                  (oed-listcollect .examples 'text "/; /")
                                                  "/"
                                                  ))
                         " ")
        (oed-clprint (make-string depth ? ) "- "))
      (oed-clprint
       (if (or .examples .regions .domains .registers) "" "*Related* ")
       (if (or .regions .domains .registers) (concat "[" (string-join (append .domains (append .regions (append .registers nil))) ", " ) "]") "" )
       )
      (oed-cprint
       "\n"
       (make-string (+ 2 depth) ? )
       "- "
       (unescape-string (oed-listcollect .synonyms 'text))
       )
      (when  (and .subsenses (oed-find-synonyms .subsenses))
       (oed-cprint "")
        (mapc (lambda(s)(oed-expand-synonym s (+ 3 depth))) .subsenses)
        )
      )
    )
  )

(defun oed-expand-antonym (raw &optional depth)
  "Search a sense sub-tree (RAW) for antonyms, the examples associated with it, and recursively expand any sub-senses, outputing as we go.
Add indentation and stars appropriate to an org entry of DEPTH."
  (or depth (setq depth 2))
  (let-alist raw
    (when .antonyms
      (if .examples
          (oed-clprint "\n"
                       (make-string depth ?*)
                       (unescape-string (concat " /"
                                                (oed-listcollect .examples 'text "/; /")
                                                "/"
                                                ))
                       " ")
        (oed-clprint (make-string depth ? ) "- "))
      (oed-clprint
       (if (or .examples .regions .domains .registers) "" "*Related* ")
       (if (or .regions .domains .registers) (concat "[" (string-join (append .domains (append .regions (append .registers nil))) ", " ) "]") "" )
       )
      (oed-cprint
       "\n"
       (make-string (+ 2 depth) ? )
       "- "
       (unescape-string (oed-listcollect .antonyms 'text))
       )
      (when (and .subsenses (oed-find-antonyms .subsenses))
        (oed-cprint "")
        (mapc (lambda(s)(oed-expand-antonym s (+ 3 depth))) .subsenses)
        )
      )
    )
  )

(defun oed-find-synonyms (forms)
  "Does FORMS contain any synonyms or subsenses with synonyms?"
  (let ((i 0)
        (flag nil))
    (while (and (not flag) (< i (length forms)))
      (let-alist (aref forms i)
        (if .synonyms
            (setq flag t)
          (setq flag (and .subsenses (oed-find-synonyms .subsenses)))
          )
        )
      (setq i (1+ i))
      )
    (identity flag)
    )
  )

(defun oed-find-antonyms (forms)
  "Does FORMS contain any antonyms or subsenses with antonyms?"
  (let ((i 0)
        (flag nil))
    (while (and (not flag) (< i (length forms)))
      (let-alist (aref forms i)
        (if .antonyms
            (setq flag t)
          (setq flag (and .subsenses (oed-find-antonyms .subsenses)))
          )
        )
      (setq i (1+ i))
      )
    (identity flag)
    )
  )

(defun oed-try-synonyms (lexicalForm homograph)
  "Find any synonyms or antonyms for the given LEXICALFORM and HOMOGRAPH of the current word."
  (interactive)
  (setq homograph (mod (string-to-number homograph) 100))
  (let ((return nil)
        (items (oed-jpath oed-cache '(0 lexicalEntries)))
        (homographclass (truncate homograph 100)))
    (mapc (lambda (x)
            (progn
              (if (equal (alist-get 'lexicalCategory x nil) lexicalForm)
                  (setq return (oed-jpath x '(entries 0)))))) items
                  )
    (when return
      (let-alist return
        (when (and (oed-find-synonyms .senses)
                   (or (= 0 (string-to-number .homographNumber))
                       (= (mod (string-to-number .homographNumber) 100) homograph)))
          (oed-cprint "\n** Synonyms ")
          (mapc (lambda(sense)
                  (oed-expand-synonym sense 3)) .senses))
        (when (and (oed-find-antonyms .senses)
                   (or (= 0 (string-to-number .homographNumber))
                       (= (mod (string-to-number .homographNumber) 100) homograph)))
          (oed-cprint "\n** Antonyms ")
          (mapc (lambda(sense)
                  (oed-expand-antonym sense 3)) .senses ))))))

(defun oed-expand-pronunciations(list)
  "Try to create org links to audio files, otherwise just
show phonetic text for word."
  (mapc (lambda(p)
          (let-alist p
            (oed-cprint "   - Dialects: " (string-join .dialects ", "))
            (cond ((and .phoneticSpelling .audioFile)
                   (oed-cprint "     - [[" .audioFile "][" (unescape-string .phoneticSpelling) "]]"))
                  ((or .audioFile)
                   (oed-cprint "     - [[" .audioFile "][" (oed-word) "]]"))
                  ((or .phoneticSpelling)
                   (oed-cprint "     - " (unescape-string .phoneticSpelling) " ")))
            )
          ) list)
  )

(defun oed-listcollect(a-list key &optional delim)
  "Take A-LIST and make a new list for any elements with a key matching KEY.
  Join this list into a string using DELIM as a separator ( '. ' as default)"
  (or delim (setq delim ", "))
  (let (result)
    (setq result
          (mapcar (lambda(i)
                    (alist-get key i)) a-list))
    (string-join result delim)))

(defun oed-paircollect(a-list key1 key2 &optional delimPairs delimSets)
  "Take A-LIST and make a new list of pairse for any elements with a key matching KEY2
and a label KEY1. Separate the pairs with DELIMPAIRS  (': ' as default
Join this list into a string using DELIMSETS as a separator ( '. ' as default)"
  (or delimPairs (setq delimPairs ": "))
  (or delimSets (setq delimSets ", "))
  (let (result)
    (setq result
          (mapcar (lambda(i)
                    (concat (alist-get key1 i) delimPairs (alist-get key2 i))) a-list))
    (string-join result delimSets)))

(defun oed-expand-entry (raw)
  "Print the basics information about an individual word usage, parsing out the RAW data."
  (let-alist raw
    (when .pronunciations
      (oed-cprint "  - Pronunciation: ")
      (oed-expand-pronunciations .pronunciations))
    (if .etymologies
        (oed-cprint "  - Etymology: " (unescape-string (oed-vhead .etymologies ))))
    (if .grammaticalFeatures
        (oed-cprint "  - Grammar:\n     " (unescape-string (oed-paircollect .grammaticalFeatures 'type 'text))))
    (if .variantForms
        (oed-cprint "  - Also: " (unescape-string (oed-listcollect .variantForms  'text))))
    (when .notes
      (mapc (lambda (n) (oed-cprint "\n *Note:* " (unescape-string(alist-get 'text n)))) .notes))
    (oed-cprint "")
    (mapc 'oed-expand-sense .senses)
  ))

(defun oed-bufferset()
  "Things to do to the buffer after it's filled with the OED data."
  (indent-region (point-min) (point-max))
  (use-local-map (copy-keymap org-mode-map))
  (local-set-key "q" 'quit-window)
  (org-shifttab 3)
  )

(defun oed-quickword ()
  "Look up the word at point and put the result in a buffer of its own."
  (interactive)
  (let (
        (theword
         (downcase
          (if (use-region-p)
              (buffer-substring (region-beginning) (region-end))
            (word-at-point))))
        (temp-buffer-setup-hook)
        (temp-buffer-show-hook)
        )
    (add-hook 'temp-buffer-setup-hook 'org-mode)
    (add-hook 'temp-buffer-show-hook 'oed-bufferset)
    (with-output-to-temp-buffer "*word-meanings*"
      (oed-cprint "#+TITLE: " theword)
      (oed-lookup-word theword)
      (unless oed-cache
        (sleep-for 1)
        (setq theword (oed-lemma theword))
        (if theword
            (oed-lookup-word theword))
        )
      (if oed-cache
          (progn
            (let ((oed-main-entry oed-cache))
              (oed-lookup-synonyms theword)
              (oed-cprint "#+SUBTITLE: " theword)
              (let ((forms (oed-jpath oed-main-entry '(0 lexicalEntries))))
                (mapc (lambda(form)
                        (let-alist form
                          (mapc (lambda(e)
                                  (oed-cprint "\n")
                                  (oed-cprint "* " .lexicalCategory)
                                  (when .pronunciations
                                    (oed-cprint "  - Pronunciation: ")
                                    (oed-expand-pronunciations .pronunciations))
                                  (when .derivativeOf
                                    (oed-cprint "  - Form of: " (oed-jpath .derivativeOf '(0 text))))
                                  (if .text
                                      (oed-cprint "  - Capitalisation: " (unescape-string .text)))
                                  (oed-expand-entry e)
                                  (oed-try-synonyms .lexicalCategory (oed-jpath e '(homographNumber)))
                                  ) .entries)
                          )
                        ) forms)
                (oed-cprint "\n* Raw")
                (oed-cprint (oed-wrap (pp-to-string forms) "#+BEGIN_SRC javascript\n" "#+END_SRC"))
                )
              )
            )
            (oed-cprint theword " not found!")
        ))))

(defun mplayer-mp3-link()
  "See if we're on an org-link to an mp3 and, if so, play it using mplayer"
  (save-excursion
    (when (and (looking-back (concat "\\[\\[\\(?:\\(http:[^][\n\r]+\\.mp3\\)?\\]\\[\\)?[^]]*\\(\\]\\)?") 0)
           (goto-char (match-beginning 0))
           (looking-at (concat "\\[\\[\\(http:[^][\n\r]+\\.mp3\\)\\]")))
      (if (match-string-no-properties 1)
          (progn
            (start-process "" nil "mplayer" (match-string-no-properties 1)) t)
        (nil)
      )))
  )

(add-hook 'org-open-at-point-functions 'mplayer-mp3-link)

(provide 'oed-org)
;;; oed-org.el ends here

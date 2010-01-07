;;; cus-load.el --- automatically extracted custom dependencies
;;
;;; Code:

(put 'url-history 'custom-loads '(url-history))
(put 'url 'custom-loads '(url-vars url-cookie url-gw url-history url-irc url-news))
(put 'url-cache 'custom-loads '(url-vars url-cache))
(put 'url-gateway 'custom-loads '(url-gw))
(put 'url-mime 'custom-loads '(url-vars))
(put 'url-hairy 'custom-loads '(url-vars url-util))
(put 'i18n 'custom-loads '(url-vars))
(put 'url-cookie 'custom-loads '(url-cookie))
(put 'hypermedia 'custom-loads '(url-vars))
(put 'url-file 'custom-loads '(url-cache url-cookie url-vars))
;; These are for handling :version.  We need to have a minimum of
;; information so `customize-changed-options' could do its job.

;; For groups we set `custom-version', `group-documentation' and
;; `custom-tag' (which are shown in the customize buffer), so we
;; don't have to load the file containing the group.

;; `custom-versions-load-alist' is an alist that has as car a version
;; number and as elts the files that have variables or faces that
;; contain that version. These files should be loaded before showing
;; the customization buffer that `customize-changed-options'
;; generates.

;; This macro is used so we don't modify the information about
;; variables and groups if it's already set. (We don't know when
;; cus-load.el is going to be loaded and at that time some of the
;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))


(defvar custom-versions-load-alist nil
 "For internal use by custom.")

(provide 'cus-load)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cus-load.el ends here

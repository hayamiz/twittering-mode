;;; org-twittering.el --- orgmode integration for twittering-mode

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Danie Roux <danie@danieroux.com>
;; Created: Dec 5, 2013
;; Version: 0.0.1
;; Keywords: twitter orgmode
;; URL: http://twmode.sf.net/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Make tweets in twittering-mode be storable via org-store-link

;;; Code:

(defun org-twittering-store-link ()
  "Stores the web link to a tweet"
  (interactive)
  (if (eq major-mode 'twittering-mode)
      (let* ((uri (twittering-get-uri-at-point))
	     (tweet (twittering-get-tweet-at-point)))
	(org-store-link-props
	 :description tweet
	 :link uri
	 :type "http")
	uri)))

(add-hook 'org-store-link-functions 'org-twittering-store-link)

(provide 'org-twittering)

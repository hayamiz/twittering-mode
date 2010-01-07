
;;;### (autoloads (url-register-auth-scheme url-get-authentication)
;;;;;;  "url-auth" "url-auth.el" (19270 18926))
;;; Generated autoloads from url-auth.el

(autoload 'url-get-authentication "url-auth" "\
Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol 'any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol 'any'
       to specify that any authentication is acceptable.  If requesting 'any'
       the strongest matching authentication will be returned.  If this is
       wrong, its no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache

\(fn URL REALM TYPE PROMPT &optional ARGS)" nil nil)

(autoload 'url-register-auth-scheme "url-auth" "\
Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.   This
         should be the same thing you expect to get returned in an Authenticate
         header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.  This
         defaults to `url-?-auth', where ? is TYPE
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned.

\(fn TYPE &optional FUNCTION RATING)" nil nil)

;;;***

;;;### (autoloads (url-cache-expired url-cache-extract url-is-cached
;;;;;;  url-store-in-cache) "url-cache" "url-cache.el" (15437 42793))
;;; Generated autoloads from url-cache.el

(autoload 'url-store-in-cache "url-cache" "\
Store buffer BUFF in the cache.

\(fn &optional BUFF)" nil nil)

(autoload 'url-is-cached "url-cache" "\
Return non-nil if the URL is cached.

\(fn URL)" nil nil)

(autoload 'url-cache-extract "url-cache" "\
Extract FNAM from the local disk cache

\(fn FNAM)" nil nil)

(autoload 'url-cache-expired "url-cache" "\
Return t iff a cached file has expired.

\(fn URL MOD)" nil nil)

;;;***

;;;### (autoloads (url-cid) "url-cid" "url-cid.el" (15092 11246))
;;; Generated autoloads from url-cid.el

(autoload 'url-cid "url-cid" "\
Not documented

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-cookie-setup-save-timer url-cookie-handle-set-cookie
;;;;;;  url-cookie-retrieve url-cookie-write-file url-cookie-parse-file)
;;;;;;  "url-cookie" "url-cookie.el" (15806 40683))
;;; Generated autoloads from url-cookie.el

(autoload 'url-cookie-parse-file "url-cookie" "\
Not documented

\(fn &optional FNAME)" nil nil)

(autoload 'url-cookie-write-file "url-cookie" "\
Not documented

\(fn &optional FNAME)" nil nil)

(autoload 'url-cookie-retrieve "url-cookie" "\
Retrieves all the netscape-style cookies for a specified HOST and PATH

\(fn HOST PATH &optional SECURE)" nil nil)

(autoload 'url-cookie-handle-set-cookie "url-cookie" "\
Not documented

\(fn STR)" nil nil)

(autoload 'url-cookie-setup-save-timer "url-cookie" "\
Reset the cookie saver timer.

\(fn)" t nil)

;;;***

;;;### (autoloads (url-dav-vc-registered url-dav-file-name-completion
;;;;;;  url-dav-file-name-all-completions url-dav-rename-file url-dav-make-directory
;;;;;;  url-dav-file-directory-p url-dav-directory-files url-dav-delete-file
;;;;;;  url-dav-delete-directory url-dav-save-resource url-dav-file-attributes
;;;;;;  url-dav-unlock-resource url-dav-active-locks url-dav-lock-resource
;;;;;;  url-dav-get-properties url-dav-supported-p) "url-dav" "url-dav.el"
;;;;;;  (15450 54377))
;;; Generated autoloads from url-dav.el

(autoload 'url-dav-supported-p "url-dav" "\
Not documented

\(fn URL)" nil nil)

(autoload 'url-dav-get-properties "url-dav" "\
Return properties for URL, up to DEPTH levels deep.

Returns an assoc list, where the key is the filename (possibly a full
URI), and the value is a standard property list of DAV property
names (ie: DAV:resourcetype).

\(fn URL &optional ATTRIBUTES DEPTH NAMESPACES)" nil nil)

(autoload 'url-dav-lock-resource "url-dav" "\
Request a lock on URL.  If EXCLUSIVE is non-nil, get an exclusive lock.
Optional 3rd argument DEPTH says how deep the lock should go, default is 0
\(lock only the resource and none of its children).

Returns a cons-cell of (SUCCESSFUL-RESULTS . FAILURE-RESULTS).
SUCCESSFUL-RESULTS is a list of (URL STATUS locktoken).
FAILURE-RESULTS is a list of (URL STATUS).

\(fn URL EXCLUSIVE &optional DEPTH)" nil nil)

(autoload 'url-dav-active-locks "url-dav" "\
Return an assoc list of all active locks on URL.

\(fn URL &optional DEPTH)" nil nil)

(autoload 'url-dav-unlock-resource "url-dav" "\
Release the lock on URL represented by LOCK-TOKEN.
Returns `t' iff the lock was successfully released.

\(fn URL LOCK-TOKEN)" nil nil)

(autoload 'url-dav-file-attributes "url-dav" "\
Not documented

\(fn URL)" nil nil)

(autoload 'url-dav-save-resource "url-dav" "\
Save OBJ as URL using WebDAV.
URL must be a fully qualified URL.
OBJ may be a buffer or a string.

\(fn URL OBJ &optional CONTENT-TYPE LOCK-TOKEN)" nil nil)

(autoload 'url-dav-delete-directory "url-dav" "\
Delete the WebDAV collection URL.
If optional second argument RECURSIVE is non-nil, then delete all
files in the collection as well.

\(fn URL &optional RECURSIVE LOCK-TOKEN)" nil nil)

(autoload 'url-dav-delete-file "url-dav" "\
Delete file named URL.

\(fn URL &optional LOCK-TOKEN)" nil nil)

(autoload 'url-dav-directory-files "url-dav" "\
Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.

\(fn URL &optional FULL MATCH NOSORT FILES-ONLY)" nil nil)

(autoload 'url-dav-file-directory-p "url-dav" "\
Return t if URL names an existing DAV collection.

\(fn URL)" nil nil)

(autoload 'url-dav-make-directory "url-dav" "\
Create the directory DIR and any nonexistent parent dirs.

\(fn URL &optional PARENTS)" nil nil)

(autoload 'url-dav-rename-file "url-dav" "\
Not documented

\(fn OLDNAME NEWNAME &optional OVERWRITE)" nil nil)

(autoload 'url-dav-file-name-all-completions "url-dav" "\
Return a list of all completions of file name FILE in directory DIRECTORY.
These are all file names in directory DIRECTORY which begin with FILE.

\(fn FILE URL)" nil nil)

(autoload 'url-dav-file-name-completion "url-dav" "\
Complete file name FILE in directory DIRECTORY.
Returns the longest string
common to all file names in DIRECTORY that start with FILE.
If there is only one and FILE matches it exactly, returns t.
Returns nil if DIR contains no name starting with FILE.

\(fn FILE URL)" nil nil)

(autoload 'url-dav-vc-registered "url-dav" "\
Not documented

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-file) "url-file" "url-file.el" (15555 54384))
;;; Generated autoloads from url-file.el

(autoload 'url-file "url-file" "\
Handle file: and ftp: URLs.

\(fn URL CALLBACK CBARGS)" nil nil)

;;;***

;;;### (autoloads (url-open-stream url-gateway-nslookup-host) "url-gw"
;;;;;;  "url-gw.el" (15555 55126))
;;; Generated autoloads from url-gw.el

(autoload 'url-gateway-nslookup-host "url-gw" "\
Attempt to resolve the given HOST using nslookup if possible.

\(fn HOST)" t nil)

(autoload 'url-open-stream "url-gw" "\
Open a stream to HOST, possibly via a gateway.
Args per `open-network-stream'.
Will not make a connexion if `url-gateway-unplugged' is non-nil.

\(fn NAME BUFFER HOST SERVICE)" nil nil)

;;;***

;;;### (autoloads (url-insert-file-contents url-file-local-copy url-copy-file
;;;;;;  url-setup-file-name-handlers) "url-handlers" "url-handlers.el"
;;;;;;  (16123 16217))
;;; Generated autoloads from url-handlers.el

(autoload 'url-setup-file-name-handlers "url-handlers" "\
Setup file-name handlers.

\(fn)" nil nil)

(autoload 'url-copy-file "url-handlers" "\
Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil.

\(fn URL NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME)" nil nil)

(autoload 'url-file-local-copy "url-handlers" "\
Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible.

\(fn URL &rest IGNORED)" nil nil)

(autoload 'url-insert-file-contents "url-handlers" "\
Not documented

\(fn URL &optional VISIT BEG END REPLACE)" nil nil)

;;;***

;;;### (autoloads (url-history-save-history url-history-parse-history
;;;;;;  url-history-setup-save-timer) "url-history" "url-history.el"
;;;;;;  (15092 12080))
;;; Generated autoloads from url-history.el

(autoload 'url-history-setup-save-timer "url-history" "\
Reset the history list timer.

\(fn)" t nil)

(autoload 'url-history-parse-history "url-history" "\
Parse a history file stored in FNAME.

\(fn &optional FNAME)" nil nil)

(autoload 'url-history-save-history "url-history" "\
Write the global history file into `url-history-file'.
The type of data written is determined by what is in the file to begin
with.  If the type of storage cannot be determined, then prompt the
user for what type to save as.

\(fn &optional FNAME)" t nil)

;;;***

;;;### (autoloads (url-http-options url-http-file-attributes url-http-file-exists-p
;;;;;;  url-http) "url-http" "url-http.el" (19270 18926))
;;; Generated autoloads from url-http.el

(autoload 'url-http "url-http" "\
Retrieve URL via HTTP asynchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.
When retrieval is completed, the function CALLBACK is executed with
CBARGS as the arguments.

\(fn URL CALLBACK CBARGS)" nil nil)

(autoload 'url-http-file-exists-p "url-http" "\
Not documented

\(fn URL)" nil nil)

(defalias 'url-http-file-readable-p 'url-http-file-exists-p)

(autoload 'url-http-file-attributes "url-http" "\
Not documented

\(fn URL)" nil nil)

(autoload 'url-http-options "url-http" "\
Returns a property list describing options available for URL.
This list is retrieved using the `OPTIONS' HTTP method.

Property list members:

methods
  A list of symbols specifying what HTTP methods the resource
  supports.

dav
  A list of numbers specifying what DAV protocol/schema versions are
  supported.

dasl
  A list of supported DASL search types supported (string form)

ranges
  A list of the units available for use in partial document fetches.

p3p
  The `Platform For Privacy Protection' description for the resource.
  Currently this is just the raw header contents.  This is likely to
  change once P3P is formally supported by the URL package or
  Emacs/W3.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-irc) "url-irc" "url-irc.el" (14435 25453))
;;; Generated autoloads from url-irc.el

(autoload 'url-irc "url-irc" "\
Not documented

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-ldap) "url-ldap" "url-ldap.el" (14398 30982))
;;; Generated autoloads from url-ldap.el

(autoload 'url-ldap "url-ldap" "\
Not documented

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-mailto url-mail) "url-mailto" "url-mailto.el"
;;;;;;  (15293 59398))
;;; Generated autoloads from url-mailto.el

(autoload 'url-mail "url-mailto" "\
Not documented

\(fn &rest ARGS)" t nil)

(autoload 'url-mailto "url-mailto" "\
Handle the mailto: URL syntax.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-data url-generic-emulator-loader url-info
;;;;;;  url-man) "url-misc" "url-misc.el" (15556 36223))
;;; Generated autoloads from url-misc.el

(autoload 'url-man "url-misc" "\
Fetch a Unix manual page URL.

\(fn URL)" nil nil)

(autoload 'url-info "url-misc" "\
Fetch a GNU Info URL.

\(fn URL)" nil nil)

(autoload 'url-generic-emulator-loader "url-misc" "\
Not documented

\(fn URL)" nil nil)

(defalias 'url-rlogin 'url-generic-emulator-loader)

(defalias 'url-telnet 'url-generic-emulator-loader)

(defalias 'url-tn3270 'url-generic-emulator-loader)

(autoload 'url-data "url-misc" "\
Fetch a data URL (RFC 2397).

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-snews url-news) "url-news" "url-news.el" (15114
;;;;;;  36876))
;;; Generated autoloads from url-news.el

(autoload 'url-news "url-news" "\
Not documented

\(fn URL)" nil nil)

(autoload 'url-snews "url-news" "\
Not documented

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-ns-user-pref url-ns-prefs isInNet isResolvable
;;;;;;  dnsResolve dnsDomainIs isPlainHostName) "url-ns" "url-ns.el"
;;;;;;  (14913 8114))
;;; Generated autoloads from url-ns.el

(autoload 'isPlainHostName "url-ns" "\
Not documented

\(fn HOST)" nil nil)

(autoload 'dnsDomainIs "url-ns" "\
Not documented

\(fn HOST DOM)" nil nil)

(autoload 'dnsResolve "url-ns" "\
Not documented

\(fn HOST)" nil nil)

(autoload 'isResolvable "url-ns" "\
Not documented

\(fn HOST)" nil nil)

(autoload 'isInNet "url-ns" "\
Not documented

\(fn IP NET MASK)" nil nil)

(autoload 'url-ns-prefs "url-ns" "\
Not documented

\(fn &optional FILE)" nil nil)

(autoload 'url-ns-user-pref "url-ns" "\
Not documented

\(fn KEY &optional DEFAULT)" nil nil)

;;;***

;;;### (autoloads (url-generic-parse-url url-recreate-url) "url-parse"
;;;;;;  "url-parse.el" (15288 22758))
;;; Generated autoloads from url-parse.el

(autoload 'url-recreate-url "url-parse" "\
Not documented

\(fn URLOBJ)" nil nil)

(autoload 'url-generic-parse-url "url-parse" "\
Return a vector of the parts of URL.
Format is:
\[proto username password hostname portnumber file reference attributes fullp]

\(fn URL)" nil nil)

;;;***

;;;### (autoloads (url-setup-privacy-info) "url-privacy" "url-privacy.el"
;;;;;;  (15293 59778))
;;; Generated autoloads from url-privacy.el

(autoload 'url-setup-privacy-info "url-privacy" "\
Not documented

\(fn)" t nil)

;;;***

;;;### (autoloads (url-view-url url-truncate-url-for-viewing url-file-extension
;;;;;;  url-hexify-string url-unhex-string url-parse-query-string
;;;;;;  url-basepath url-percentage url-display-percentage url-pretty-length
;;;;;;  url-strip-leading-spaces url-eat-trailing-space url-get-normalized-date
;;;;;;  url-lazy-message url-normalize-url url-insert-entities-in-string
;;;;;;  url-parse-args url-debug url-debug) "url-util" "url-util.el"
;;;;;;  (19270 18926))
;;; Generated autoloads from url-util.el

(defvar url-debug nil "\
*What types of debug messages from the URL library to show.
Debug messages are logged to the *URL-DEBUG* buffer.

If t, all messages will be logged.
If a number, all messages will be logged, as well shown via `message'.
If a list, it is a list of the types of messages to be logged.")

(autoload 'url-debug "url-util")

(autoload 'url-debug "url-util" "\
Not documented

\(fn TAG &rest ARGS)" nil nil)

(autoload 'url-parse-args "url-util" "\
Not documented

\(fn STR &optional NODOWNCASE)" nil nil)

(autoload 'url-insert-entities-in-string "url-util" "\
Convert HTML markup-start characters to entity references in STRING.
Also replaces the \" character, so that the result may be safely used as
  an attribute value in a tag.  Returns a new string with the result of the
  conversion.  Replaces these characters as follows:
    &  ==>  &amp;
    <  ==>  &lt;
    >  ==>  &gt;
    \"  ==>  &quot;

\(fn STRING)" nil nil)

(autoload 'url-normalize-url "url-util" "\
Return a 'normalized' version of URL.
Strips out default port numbers, etc.

\(fn URL)" nil nil)

(autoload 'url-lazy-message "url-util" "\
Just like `message', but is a no-op if called more than once a second.
Will not do anything if url-show-status is nil.

\(fn &rest ARGS)" nil nil)

(autoload 'url-get-normalized-date "url-util" "\
Return a 'real' date string that most HTTP servers can understand.

\(fn &optional SPECIFIED-TIME)" nil nil)

(autoload 'url-eat-trailing-space "url-util" "\
Remove spaces/tabs at the end of a string.

\(fn X)" nil nil)

(autoload 'url-strip-leading-spaces "url-util" "\
Remove spaces at the front of a string.

\(fn X)" nil nil)

(autoload 'url-pretty-length "url-util" "\
Not documented

\(fn N)" nil nil)

(autoload 'url-display-percentage "url-util" "\
Not documented

\(fn FMT PERC &rest ARGS)" nil nil)

(autoload 'url-percentage "url-util" "\
Not documented

\(fn X Y)" nil nil)

(autoload 'url-basepath "url-util" "\
Return the base pathname of FILE, or the actual filename if X is true.

\(fn FILE &optional X)" nil nil)

(autoload 'url-parse-query-string "url-util" "\
Not documented

\(fn QUERY &optional DOWNCASE)" nil nil)

(autoload 'url-unhex-string "url-util" "\
Remove %XXX embedded spaces, etc in a url.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding.

\(fn STR &optional ALLOW-NEWLINES)" nil nil)

(autoload 'url-hexify-string "url-util" "\
Escape characters in a string.

\(fn STR)" nil nil)

(autoload 'url-file-extension "url-util" "\
Return the filename extension of FNAME.
If optional variable X is t,
then return the basename of the file with the extension stripped off.

\(fn FNAME &optional X)" nil nil)

(autoload 'url-truncate-url-for-viewing "url-util" "\
Return a shortened version of URL that is WIDTH characters or less wide.
WIDTH defaults to the current frame width.

\(fn URL &optional WIDTH)" nil nil)

(autoload 'url-view-url "url-util" "\
View the current document's URL.
Optional argument NO-SHOW means just return the URL, don't show it in
the minibuffer.

This uses `url-current-object', set locally to the buffer.

\(fn &optional NO-SHOW)" t nil)

;;;***

;;;### (autoloads nil "url" "url.el" (19270 18926))
;;; Generated autoloads from url.el

(defvar url-configuration-directory "~/.url")

;;;***

(provide 'url-auto)

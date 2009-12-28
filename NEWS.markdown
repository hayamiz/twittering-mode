
# NEWS -- The history of twittering-mode

## 0.9: 2009-12-29

### Improvements

* Moved to GitHub.
* Brand new web site in English and Japanese.
* Secure sessions via HTTPS if 'cURL' is available.
* Lists support.
* Follow/remove support.
* Favorite/unfavorite support.
* Hash tag support.
* Direct message support.
* On-the-spot TinyURL-ize(tinyurl or to.ly).
* On-the-fly tweet length check.
* ReTweet format string.
* Remembers visited user names and tweets.
* Parameterized the number of tweets retrieved at a time.
* Walk through items(username, URL, etc) by TAB.
* Image type discriminant with 'file' program if available.
* More descriptive error messages on minibuffer.
* Do not discard old timeline data as possible.
* Suspend by hitting 'q'.
* Image file resize with 'convert' program(imagemagick) if available.
* Unit test framework was introduced.
* Support 'Proxy-connection: Keep-Alive'.

### Bug fixes

* URL cannot not be opened by mouse-click or Enter.
* Don't set the original status ID when retweeting.
* Wrong regexp for searching URL in status text.
* Username extraction from status text.
* Update mode-line immediately.
* Use a temporary buffer for each HTTP session.
* Use MD5 hash to distinguish image files with the same name.
* Check whether temp buffers are alive in sentinels before killing them.
* And some typos.

### Thanks to

* Alberto Garcia
* Jaemok Jeong
* Kouhei Sutou
* Naohiro Aota
* Satoshi Yatagawa
* Tadasohi MATSUO
* Thomas Danckaert
* Tsuyoshi CHO
* IMAI Toshiyuki

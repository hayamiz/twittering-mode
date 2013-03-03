 Twittering-mode: a Twitter client for Emacs
=============================================

Twittering-mode enables you to twit on Emacsen.

- web: http://twmode.sf.net
- github: http://github.com/hayamiz/twittering-mode

 Features
----------

* Activities on Twitter
  * Viewing various timelines
    * Home timeline
    * Replies
    * User's timeline
    * Public timeline
    * Favorites timeline
    * Retweets timeline
    * Merged timeline
    * Timeline without tweets satisfying a condition
  * Posting tweets
    * Direct message
    * ReTweet
    * Hash tag
    * Signature
  * Following and removing users
  * Marking tweets as favorites
* HTTP Proxy support
* Secure connection via HTTPS (cURL, GNU Wget, OpenSSL or GnuTLS is required)

 Supported Emacsen
-------------------

- GNU Emacs 21 (some restrictions)
- GNU Emacs 22, 23, 24

 Prerequisites
-------------------

- For SSL connection, one of the followings is required.
  SSL connection is required for the Twitter REST API v1.1.
  - cURL http://curl.haxx.se/
  - GNU Wget http://www.gnu.org/software/wget/
  - OpenSSL http://www.openssl.org/
  - GnuTLS http://www.gnu.org/software/gnutls/
- For parsing JSON, json.el is required.
  It is distributed with Emacs 23.1 and later.
- For keeping an OAuth authorized token in a local storage encrypted
  with master password, GnuPG ( http://www.gnupg.org/ ) is  required.
  On Emacs 22 and earlier, either EasyPG ( http://epg.sourceforge.jp/ )
  or alpaca.el( http://www.mew.org/~kazu/proj/cipher/ ) is also
  required.
- For displaying icons in formats that are not supported by Emacs and
  resizing icon images, ImageMagick ( http://www.imagemagick.org/ ) or
  its compatible alternative is required, e.g. GraphicsMagick
  ( http://www.graphicsmagick.org/ ). Note that twittering-mode on
  icon-mode converts retrieved icons into XPM in default. So,
  icon-mode without additional configuration requires, ImageMagick (or
  its alternative).
- For keeping retrieved icons in a local storage, gzip
  ( http://www.gzip.org/ ) is required.

 Quick start
------------------------

0.  Put *twittering-mode.el* in a directory specified by the variable
    `load-path`. Note that the directories *emacs21* and *url-emacs21*
    must be placed at the same directory on Emacs 21. On Windows
    without curl or wget, the directory *win-curl* must be placed
    there. You can add a directory to the variable `load-path` by
    `(add-to-list 'load-path "ADDITIONAL-DIRECTORY")`.
1.  Execute `M-x twit` to run twittering-mode.
2.  Open OAuth authorization page with an external browser, click *Allow*,
    and enter the PIN code in the prompt of Emacs.

    If you have introduced the configuration
    `(setq twittering-use-master-password t)`, twittering-mode will
    ask you a master password and it will write the authorized OAuth
    token into *~/.twittering-mode.gpg* (in default) with encryption.
    Once the authorized OAuth token is encrypted, you do not have to
    retrieve a PIN code with an external browser. The master password
    is only required to establish authorized connection to Twitter.
3.  Your home timeline will appear. Basic key bindings are as
    follows.
    - `V`: Open or switch to another timeline by
      [timeline-spec](#timeline-spec).
    - `u` or `C-cC-s`: Post a tweet.
    - `RET`: Post a reply to the pointed tweet or open the pointed URL
      with `browse-url`.
    - `C-c RET`: Post an organic retweet. This is only a tweet citing
      the pointed tweet and not an official/native retweet.
    - `C-uC-c RET`: Post an official/native retweet.
    - `d`: Send a direct message.
    - `C-cC-w`: Delete the pointed tweet.
4.  Add some of the following major configurations to your init file
    if you like.
    - To display icons, add `(setq twittering-icon-mode t)`.
      This may require ImageMagick or its compatible alternative.
    - To resize icons, add `(setq twittering-convert-fix-size SIZE)`.
      The default size is 48 pixels. This requires ImageMagick or its
      compatible alternative.
    - To keep retrieved icons in a local storage, add
      `(setq twittering-use-icon-storage t)`. This requires gzip. The
      icons are saved on *~/.twittering-mode-icons.gz*, which can be
      changed by the variable `twittering-icon-storage-file`.
    - To display tweets in the *reverse* order, add
      `(setq twittering-reverse-mode t)`. With it, the latest tweet
      is rendered the bottom of the buffer.
    - To use a HTTP proxy, add the following configuration.
      <pre><code>
        (setq twittering-proxy-use t)
        (setq twittering-proxy-server "PROXY-HOSTNAME")
        (setq twittering-proxy-port PROXY-PORT-NUMBER)
      </code></pre>
    - To change the number of tweets retrieved at once, add
      `(setq twittering-number-of-tweets-on-retrieval NUMBER)`.
    - To change the interval of retrieving tweets, add
      `(setq twittering-timer-interval SECOND)`. You should be careful
      not to exceed the limitation of number of API calls.
    - To display the number of unread tweets on the mode-line, add
      `(twittering-enable-unread-status-notifier)`.
    - To display the remaining number of API calls, add
      `(setq twittering-display-remaining t)`.
    - To change the format of an organic retweet, configure the
      variable `twittering-retweet-format`. For example, add
      `(setq twittering-retweet-format '(nil _ " %u RT @%s: %t"))`.
      For details, see the docstring of the variable by
      `M-x describe-variable`.
    - To change the timelines automatically opened on starting
      twittering-mode, configure the variable
      `twittering-initial-timeline-spec-string`. For example,
      add the below configuration.
      <pre><code>
        (setq twittering-initial-timeline-spec-string
              '("(:home+@)"
                "(:search/twittering mode/+:search/twmode/)"))
      </code></pre>
      For details, see the docstring of the variable by
      `M-x describe-variable`.
    - To customize the format of tweets, configure the variable
      `twittering-status-format`. For example, add the below
      configuration.
      <pre><code>
        (setq twittering-status-format
              "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")
      </code></pre>
      For details, see the docstring of the variable by
      `M-x describe-variable`.
    - To inherit mentions and hashtags on editing a reply, add
      `(setq twittering-edit-skeleton 'inherit-any)`.
      For details, see the docstring of the variable by
      `M-x describe-variable`.

    Enjoy!

 Usage
------------------------
- Move the cursor.
  - `0`: Go to the beginning of the line.
  - `^`: Go to the beginning of the text on the line.
  - `$`: Go to the end of the line.
  - `G`: Go to the bottom tweet.
  - `H`: Go to the top tweet.
  - `h`: Move the cursor left.
  - `j`: Go to the next tweet.
  - `k`: Go to the previous tweet.
  - `l`: Move the cursor right.
  - `n`: Go to the next tweet by the author of the pointed tweet.
  - `p`: Go to the previous tweet by the author of the pointed tweet.
  - `TAB`: Go to the next thing (link, user name, URL, etc.).
  - `M-TAB` or `<backtab>`: Go to the previous thing (link, user name,
    URL, etc.).
  - `C-v` or `SPC`: Scroll the buffer upward.
  - `M-v` or `<backspace>`: Scroll the buffer downward.
- Switch a timeline.
  - `L`: Open a list timeline interactively.
  - `V`: Open a timeline by [timeline-spec](#timeline-spec).
  - `b`: Switch to the previous timeline.
  - `f`: Switch to the next timeline.
  - `C-cC-d`: Open the direct messages timeline.
    This is equivalent to `V` with `:direct_messages`.
  - `v`: Open the home timeline of the pointed user.
  - `C-cC-f`: Open the friend timeline.
    This is equivalent to `V` with `:friends`.
  - `C-cC-q`: Open a search timeline.
    This is equivalent to `V` with `:search/QUERY/`.
  - `C-cC-r`: Open the replies timeline.
    This is equivalent to `V` with `:replies`.
  - `C-cC-u`: Open the current user timeline.
    This is equivalent to `V` with `USERNAME`, where *USERNAME* is
    your screen-name.
  - `q`: Close the current buffer.
- Update/clear the timeline.
  - `g`: Update the current timeline.
  - `C-cC-e`: Erase tweets of the current buffer from memory.
- Display replied tweets.
  - `r`: Display replied tweets or hide them.
  - `R`: Display replied tweets, retrieve the replied tweet or hide
    them.
- Post a tweet.
  - `u` or `C-cC-s`: Post a tweet.
  - `RET`: Post a reply to the pointed tweet or open the pointed URL
    with `browse-url`.
  - `C-c RET`: Post an organic retweet. This is only a tweet citing
    the pointed tweet and not an official/native retweet.
  - `C-uC-c RET`: Post an official/native retweet.
  - `d`: Send a direct message.
  - `C-cC-l`: Post a joke message.
- Delete a tweet.
  - `C-cC-w`: Delete the pointed tweet.
- View more info.
  - `U`: Push the URL of the pointed link or tweet onto the kill ring.
  - `C-cC-v`: Open the profile of the pointed user with `browse-url`.
  - `<mouse-1>`: Open the pointed URL with `browse-url`.
- Change the mode of twittering-mode.
  - `a`: Toggle the state of the buffer between active/inactive.
    Twittering-mode does not update an inactive buffer.
  - `i`: Toggle the state of the buffer between with or without icon
    images.
  - `t` or `C-cC-p`: Toggle between with or without a proxy server.
  - `C-cC-t`: Set the current hashtag.


<a id="timeline-spec">Timeline spec</a>
------------------------
By pressing *V* (`twittering-visit-timeline`) on twittering-mode, you
can specify a timeline to be opened by *timeline spec*.
A timeline spec can be used anywhere you have to specify a timeline.

The valid timeline specs follows:

- Basic timeline
  - `:home` : The home timeline.
  - `:mentions` : Tweets mentioning you.
  - `:public` : The public timeline.
  - `USER` : Tweets posted by *USER*.
  - `USER/LISTNAME` :
    The list timeline owned by *USER* and named *LISTNAME*.
- Direct message
  - `:direct_messages` : Direct messages sent to you.
  - `:direct_messages_sent` : Direct messages that you sent.
- Favorite
  - `:favorites` : Tweets that you marked as a favorite.
  - `:favorites/USER` : Tweets that *USER* marked as a favorite.
- Hashtag
  - `#HASHTAG` : Tweets including *#HASHTAG*.
- Retweet
  - `:retweeted_by_me` : Retweets that you posted.
  - `:retweeted_by_user/USER` : Retweets posted by *USER*.
  - `:retweeted_to_me` : Retweets sent to your home timeline.
  - `:retweeted_to_user/USER` :
    Retweets sent to *USER*'s home timeline.
  - `:retweets_of_me` :
    Your tweets that have been retweeted by others.
- Single
  - `:single/ID` : A tweet specified by *ID*.
- Search
  - `:search/QUERY-STRING/` :
    Tweets matching *QUERY-STRING*. In *QUERY-STRING*, */* (slash) and
    *\\* (backslash) must be escaped as *\\/* or *\\\\*, respectively.
- Alias<br />
  You can define aliases for timeline specs. By defining a short alias
  as a long timeline spec, you can refer it more easily. There are two
  types of alias, *simple* and *functional* as below. Both types are
  defined in the association list bound to the variable
  `twittering-timeline-spec-alias`.
  - `$ALIAS-NAME`
    The timeline spec bound to *ALIAS-NAME* in
    `twittering-timeline-spec-alias`.
  - `$ALIAS-NAME(ARGUMENT)`
    The timeline spec generated by calling the function, which is
    bound to *ALIAS-NAME* in `twittering-timeline-spec-alias`, with
    the argument *ARGUMENT*. The function can be specified as a symbol
    or a lambda expression.
    Functions must receive one string argument.

  As an example, consider the following definition of
  `twittering-timeline-spec-alias`.
  <pre><code>
    (setq twittering-timeline-spec-alias
          '(("FRIENDS" . "my-account/friends-list")
            ("related-to" .
             (lambda (username)
               (if username
                   (format ":search/to:%s OR from:%s OR @%s/"
                           username username username)
                 ":home")))
            ("related-to-twitter" . "$related-to(twitter)")))
  </code></pre>
  With this configuration, you can use the below aliases.
  - `$FRIENDS` is equivalent to `my-account/friends-list`.
  - `$related-to` is equivalent to `:home`.
  - `$related-to(twitterapi)` is equivalent to `:search/to:twitterapi OR from:twitterapi OR @twitterapi/`.
  - `$related-to-twitter` is equivalent to `:search/to:twitter OR from:twitter OR @twitter/`.

  You can use an alias on definition of other aliases, but an alias
  including a circular-reference is forbidden.
- Composite timeline spec
  - `(SPEC1+SPEC2)` :
    The timeline generated by merging two timelines, specified by
    *SPEC1* and *SPEC2*.
  - `:exclude-if/FUNC/SPEC` :
    The timeline equals *SPEC*, except that it does not include tweets
    that the function *FUNC* returns non-nil for. *FUNC* must be a
    function that receives an alist corresponding to a tweet as an
    argument. A lambda expression and a symbol bound to a function are
    valid as *FUNC*. But a symbol name must not include two special
    characters, "(" or "/". You can specify any timeline spec for
    *SPEC*. For example, you can ignore tweets including "WORD" from the
    home timeline by the following timeline spec;
    `:exclude-if/(lambda (tweet) (string-match "WORD" (cdr (assq 'text tweet))))/:home`.
  - `:exclude-re/REGEXP/SPEC` :
    The timeline equals *SPEC*, except that it does not include tweets
    that match the regular expression *REGEXP*. In *REGEXP*, a slash
    must be escaped with a backslash. For example, tweets including a
    slash are excluded from the timeline corresponding to the Emacs
    string literal `":exclude-re/\\//:home"`.

 Authors & Contributors
------------------------

- Y. Hayamizu
- naoya_t
- Tsuyoshi CHO
- Alberto Garcia
- Satoshi Yatagawa
- 高山智也
- Tadashi MATSUO (cvmat)
- 青田(naota)
- Jaemok Jeong(jmjeong)
- Thomas Danckaert
- IMAI Toshiyuki

 See also
----------

- http://www.emacswiki.org/emacs-en/TwitteringMode

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
- GNU Emacs 22, 23

 Prerequisites
-------------------

- For SSL connection, one of the followings is required.
  - cURL
  - GNU Wget
  - OpenSSL
  - GnuTLS
- For storing an authorized token with encryption (master password
  function), GnuPG is required.
- For converting and resizing icon images, ImageMagick or its compatible
  alternative is required.
- For persistent icon storage, gzip is required.

 Quick start
------------------------

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

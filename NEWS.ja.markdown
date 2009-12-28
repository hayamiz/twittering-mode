
# NEWS -- twittering-modeのあゆみ

## 0.9: 2009-12-29

### 改良

* GitHubへ移動
* 英語と日本語でWebサイトを開設
* cURLが利用可能なときはHTTPSを使ってセキュアに通信するように
* リスト対応
* フォロー/リムーブ対応
* お気に入り機能に対応
* ハッシュタグ対応
* ダイレクトメッセージ送信に対応
* URLをその場でTinyURL化する機能
* 入力文字列の文字数チェック
* ReTweetの形式を選択可能に
* 閲覧したユーザー名や、送信したつぶやきの履歴を保存
* 一度に取得するつぶやきの数をパラメータ化
* ユーザー名やURLなどの要素をTABキーで移動可能に
* 'file'コマンドを利用した画像の種類の判別
* よりわかりやすいエラー出力
* 取得済みのタイムラインのデータを保持するように
* サスペンド機能
* Imagemagickが利用可能なときは画像のリサイズを行うように
* 単体テストフレームワークの導入
* 'Proxy-connection: Keep-Alive'への対応

### バグ修正

* マウスクリックやEnterキーでURLを開けない問題を修正
* ReTweetのときはオリジナルIDを設定しないように
* URL抽出の正規表現を修正
* ユーザー名抽出の処理を修正
* モードラインを即時更新するよう修正
* 各HTTPセッションごとに一時バッファを利用するよう変更
* MD5ハッシュを利用して同じ名前の画像ファイルの衝突を回避
* センティネルでバッファをkillする際のエラー処理
* いくつかの誤字修正

### 感謝

* Alberto Garcia
* Jaemok Jeong
* Kouhei Sutou
* Naohiro Aota
* Satoshi Yatagawa
* Tadasohi MATSUO
* Thomas Danckaert
* Tsuyoshi CHO
* IMAI Toshiyuki


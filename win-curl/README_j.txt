cURL + OpenSSL for Windows マニュアル
                                                            Since: 27-Feb-2002
                                                                  Version: 1.1
                                                  Author: MURAOKA Taro (KoRoN)
                                                     Last Change: 04-Nov-2003.
説明
  cURLはコマンドラインで URI/URL を指定して、ファイルをダウンロードするソフト
  ウェアです。アーカイブに収録されている実行ファイル(curl.exe)は、ソースコード
  から SSL を有効にして Visual C++ 6.0 を用いてコンパイルしたものです。 SSL は
  OpenSSLのものをVC6とNASM 0.98を用いてDLLとしてコンパイルしました。
  
  使い方、及びライセンスについての詳細は以下のURIと配布アーカイブ内のdocs/*を
  参照してください。

  - cURL (7.10.8)
    http://curl.haxx.se/
  - OpenSSL (0.9.7c)
    http://www.openssl.org/

  本アーカイブは以下のURIにて配布されました。

  - 香り屋
    http://www.kaoriya.net/
  - 配布者メールアドレス
    koron@tka.att.ne.jp


旧版をお使いの方へ
  今版よりDLLを使用せず単体のEXEとしてビルドするようにしました。そのため旧版に
  含まれていた libcurl.dll, libeay32.dll, ssleay32.dllの3つのファイルは不要に
  なりました。これらのファイルは存在していても何の害にもなりませんが得にもなら
  ず、また無くてもcURLの動作に支障をきたす事はありません。よって各自の判断で消
  していただいて構いません。


更新履歴
  04-Nov-2003
    curlを7.10.8にアップデート
    OpenSSHを0.9.7cにアップデート
    CAを配布物に追加
  08-Apr-2003
    curlを7.10.4にアップデート
    OpenSSHを0.9.6iにアップデート
  23-Jan-2003
    curlを7.10.3にアップデート
    OpenSSHを0.9.6hにアップデート
    DLLではなく単体のEXEとしてリビルド
    配布アーカイブ内にdocs/FAQを追加
  05-Oct-2002
    curlを7.10にアップデート
    OpenSSHを0.9.6gに変更
  06-Aug-2002
    OpenSSHを0.9.6eに変更

------------------------------------------------------------------------------
                  生きる事への強い意志が同時に自分と異なる生命をも尊ぶ心となる
                                      MURAOKA Taro/KoRoN <koron@tka.att.ne.jp>
 vim:set ts=8 sts=2 sw=2 tw=78 et ft=memo:

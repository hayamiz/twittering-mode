#!/bin/sh

START_DELIMITER=";; #BEGIN-CERTIFICATE"
END_DELIMITER=";; #END-CERTIFICATE"

BUNDLE_FILE=`mktemp`
EMBEDDED_CERTS=`mktemp`

curl https://curl.haxx.se/ca/cacert.pem > ${BUNDLE_FILE}
(grep --before-context=1 '^=' ${BUNDLE_FILE} | sed -ne '/^[^-=]/p' \
| egrep -i '(verisign|geotrust global ca$|DigiCert High Assurance EV Root CA)' \
| while read CERT ; do
    echo ";; ${CERT}";
    NUM=`grep -c "^${CERT}\$" ${BUNDLE_FILE}`;
    sed -ne '/^'"${CERT}"'$/,/^$/p' ${BUNDLE_FILE} \
      | (for n in `seq ${NUM}` ; do
          openssl x509 -issuer -subject -serial -fingerprint -dates \
            | sed -e 's/^/;; /' \
            | sed -e '/^;; -----/,/^;; -----/s/^;; //' \
            | sed -e '/^-----BEGIN/s/^/"/' -e '/^-----END/s/$/\n"/';
        done)
  done ) > ${EMBEDDED_CERTS}

sed -i.bak -ne '/^'"${START_DELIMITER}"'$/{
a '"${START_DELIMITER}"'
r '"${EMBEDDED_CERTS}"'
a '"${END_DELIMITER}"'
}
/^'"${START_DELIMITER}"'$/,/^'"${END_DELIMITER}"'$/!p' twittering-mode.el

rm ${EMBEDDED_CERTS} ${BUNDLE_FILE}

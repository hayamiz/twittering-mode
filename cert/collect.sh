#!/bin/sh

URLS="https://www.geotrust.com/resources/root_certificates/certificates/Equifax_Secure_Certificate_Authority.pem
https://www.geotrust.com/resources/root_certificates/certificates/GeoTrust_Global_CA.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-3G2.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-3.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-3G5.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-3G3.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-3G4.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-2G3.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-2G2.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-1.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-1G3.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-universal.pem
http://www.verisign.com/repository/roots/root-certificates/PCA-4G3.pem
http://www.verisign.com/repository/roots/root-certificates/PCA_1_G6.pem
http://www.verisign.com/repository/roots/root-certificates/PCA_2_G6.pem"

wget $URLS
for n in $URLS; do
  file=${n##*/};
  (
    openssl x509 -in $file -issuer -subject -serial -fingerprint -dates -noout;
    echo "\nURL: $n\nRetrieved-at: "`TZ=UTC date --rfc-3339=seconds`;
  ) | sed -e 's/^/;; /' ;
  (cat $file | tr -d '\r' ; echo; echo ) | sed -e '1s/^/"/' -e '$s/^$/"/' \
    | tr -s '\n'
  echo ;
done

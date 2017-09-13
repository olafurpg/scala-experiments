# Extract corpus from google drive
DRIVE_ID="0B5BBsTPBxGcMemRsMjBpUDNYM1E"
mkdir -p target
curl -c /tmp/cookies "https://drive.google.com/uc?export=download&id=$DRIVE_ID" > /tmp/foo.html
curl -L -b /tmp/cookies \
  "https://drive.google.com$(cat /tmp/foo.html | grep -Po 'uc-download-link" [^>]* href="\K[^"]*' | sed 's/\&amp;/\&/g')" \
  > target/semanticdb.zip
unzip target/semanticdb.zip -d target/
mv target/semanticdbs/semanticdb.v11 target

#!/bin/bash

#cat <(tr -d '\r \t' | egrep -o '^[^#]*' - | grep '=' | egrep -v '^[[:space:]]*$')
#exit
index=""

cat <<EOF
<?xml version="1.0"?>
<testFile
  xmlns="https://testvectors.cryptolib.org/xml-schema/v0.1/block-cipher_kat" 
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="https://testvectors.cryptolib.org/xml-schema/v0.1/block-cipher_kat
                      https://testvectors.cryptolib.org/xml-schema/v0.1/block-cipher_kat.xsd">
<header>
EOF

echo "  <convertDate>$(date '+%FT%T%:z')</convertDate>"

if [ -n "$1" ]; then
	orig_name="$1"
	orig_sha256=$(sha256sum "$1" | cut -f1 -d' ')
	orig_sha512=$(sha512sum "$1" | cut -f1 -d' ')
	
	exec < "$1"
	
	echo "  <originalFilename>""$(basename "$orig_name")""</originalFilename>"
	echo "  <originalSha256>${orig_sha256}</originalSha256>"
	echo "  <originalSha512>${orig_sha512}</originalSha512>"
fi


header_done=0

kat_type="kat_vector_without_iv"

while read LINE; do
	if [ "$header_done" = "0" ]; then
		A=$(sed 's/^#[[:space:]]*\(.*\)$/\1/1' <<< "$LINE")
		if [ -n "$A" ]; then
			echo "  <comment>$A</comment>"
		else
			echo -e "</header>\n"
			header_done=1
			echo -e "<body>\n"
				fi
	else
		IFS='=' read KEY VALUE <  <(tr -d '\t ' <<< "$LINE")
		if [ -n "$VALUE" ]; then
#		echo "key: $KEY"
#		echo "value: $VALUE"
#		echo "c: $index"
		case $KEY in
			COUNT)
				index=$VALUE
				;;
			KEY)
				secret=$VALUE
				;;
			IV)
				iv=$VALUE
				kat_type="kat_vector_with_iv"
				;;
			PLAINTEXT)
				if [ $(( ${#VALUE} % 2)) -eq 1 ]; then
					plaintext=0$VALUE
				else
					plaintext=$VALUE
				fi
				;;
			CIPHERTEXT)
				if [ $(( ${#VALUE} % 2)) -eq 1 ]; then
					ciphertext=0$VALUE
				else
					ciphertext=$VALUE
				fi
				;;
			*)
				;;
		esac
		else
			if [ -n "$index" ]; then
				printf "  <%s>\n" $kat_type
				printf "    <%s>%s</%s>\n" index $index index
				printf "    <%s>%s</%s>\n" key $secret key
				if [ -n "$iv" ]; then
					printf "    <%s>%s</%s>\n" iv "$iv" iv
				fi
				printf "    <%s>%s</%s>\n" plaintext $plaintext plaintext
				printf "    <%s>%s</%s>\n" ciphertext $ciphertext ciphertext
				printf "  </%s>\n\n" $kat_type
				index=""
			fi
		fi
	fi
done <  <( tr -d '\r') 

cat <<EOF
</body>
</testFile>

EOF


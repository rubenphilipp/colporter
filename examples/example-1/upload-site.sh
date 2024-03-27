#!/bin/zsh
################################################################################
### This script uploads the site
### to a SFTP server specified via the following
### variables.
### Authorization uses a public key.
###
### 2023-07-29
################################################################################

dir=$PWD
# the path to the site directory
sitedir="$dir"/site/
# the path to the sftp key
sftpkey="$dir"/site_rsa
sftpserver=rubenphilipp.test
sftpuser=sshusername
# the output path
sftpdir=/home/sites/rubenphilipp


echo "Do you really want to upload the site to the server?\n\n"
echo "The data in the directory:"
echo "$sitedir\n"
echo "will then be uploaded to the directory:"
echo "$sftpdir\n"
echo "on the server:"
echo "$sftpserver"
echo "\n"
read -p "Press any key to confirm: "

sftp -i "$sftpkey" "$sftpuser"@"$sftpserver" <<EOF
cd $sftpdir
put -r $sitedir
EOF

echo "Done."

################################################################################
### EOF upload-site.sh

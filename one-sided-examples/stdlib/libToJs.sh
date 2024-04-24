#!/bin/bash

OUTJS="one-sided-examples/src/ImportLibs.js"
OUTPURS="one-sided-examples/src/ImportLibs.purs"

LIBSRC="libSources"
ALLVARS=()

echo "" > $OUTJS
echo "module ImportLibs ( $LIBSRC ) where" > $OUTPURS
echo "" >> $OUTPURS
echo "import Data.Tuple (Tuple(..))" >> $OUTPURS

for FILE in $(find -name "*.os") 
do
  FILENAME="$(basename $FILE)"
  LIB="${FILENAME%.*}"
  LIBLOWER=$( echo $LIB | tr '[:upper:]' '[:lower:]')
  VARNAME="$LIBLOWER"Src

  echo "" >> $OUTJS
  echo "export const $VARNAME = " >> $OUTJS
  echo "\`" >> $OUTJS
  cat $FILE >> $OUTJS
  echo "\`;" >> $OUTJS

  echo "foreign import $VARNAME :: String" >> $OUTPURS
  echo "" >> $OUTPURS

  ALLVARS+=("Tuple \"$VARNAME\" $VARNAME")
done

printf -v VARSTR '%s,' "${ALLVARS[@]}"

echo "$LIBSRC :: Array (Tuple String String)" >> $OUTPURS
echo "$LIBSRC = [" >> $OUTPURS
echo "${VARSTR%,}" >> $OUTPURS
echo "]" >> $OUTPURS

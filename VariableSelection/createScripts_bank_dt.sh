#!/bin/bash

# Script to create a rank list with respect with their IV
# Usage: 

# Write name without extension. Check script/ directory
SCRIPT=IV_DT_template_bank

# Where am I?
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

nline=$(grep -n LOGISTIC ${SCRIPT}.R | cut -d : -f 1)

if [ ${nline} -lt  0 ]; then
    echo "Scripts could not be created."
    exit 1
fi

filename=./IVRank/IV_bank_fold_5.txt

if [ -s "${filename}" ]
then 
   echo "File exists and is not empty."
else
   echo "File does not exist, or is empty."
   exit 1
fi

nvar=1
nline=$((nline+3))

mkdir ScriptsDT_F5

while read line; do
   if [ ${nvar} -eq  1 ]
   then
       sed -i ${nline}i${line} ${SCRIPT}.R
   else
       sed -i ${nline}i'+ '${line} ${SCRIPT}.R
   fi  
   cp  ${SCRIPT}.R ./ScriptsDT_F5/${SCRIPT}_${nvar}.R
   sed -i s/NVAR/${nvar}/ ./ScriptsDT_F5/${SCRIPT}_${nvar}.R 
 
   nline=$((nline+1))
   nvar=$((nvar+1)) 
done < $filename


cp ${SCRIPT}_r.R ${SCRIPT}.R 

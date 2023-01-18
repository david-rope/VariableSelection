#!/bin/bash

BATCH='qsub'

INPUTDIR=/data1/David/BashR/KFoldsCV/Higgs/ScriptsSVM_F5

# Where am I?
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INITJOB=1
NJOBS=$(ls $INPUTDIR | wc -l)

echo "Num of jobs: " $NJOBS

Run=1

mkdir -p $DIR/batchSVM_F5/

for f in $(ls $INPUTDIR/*.R)
do
RunNum=$((Run))
FILE=svm-${RunNum}-of-${NJOBS}

cat >  $DIR/batchSVM_F5/$FILE.pbs <<EOF
#!/bin/bash

#PBS -l walltime=30:00:00

Rscript ${f} 
EOF
$BATCH -o ${DIR}/batchSVM_F5/ -e ${DIR}/batchSVM_F5/ batchSVM_F5/${FILE}.pbs
echo $BATCH ${DIR}/batchSVM_F5/${FILE}.pbs
sleep 0.1
Run=$((Run+1))
done


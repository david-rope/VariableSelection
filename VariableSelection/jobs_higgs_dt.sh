#!/bin/bash

BATCH='qsub'

INPUTDIR=/data1/David/BashR/KFoldsCV/Higgs/ScriptsDT_F5

# Where am I?
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INITJOB=1
NJOBS=$(ls $INPUTDIR | wc -l)

echo "Num of jobs: " $NJOBS

Run=1

mkdir -p $DIR/batchDT_F5/

for f in $(ls $INPUTDIR/*.R)
do
RunNum=$((Run))
FILE=dt-${RunNum}-of-${NJOBS}

cat >  $DIR/batchDT_F5/$FILE.pbs <<EOF
#!/bin/bash

#PBS -l walltime=30:00:00

Rscript ${f} 
EOF
$BATCH -o ${DIR}/batchDT_F5/ -e ${DIR}/batchDT_F5/ batchDT_F5/${FILE}.pbs
echo $BATCH ${DIR}/batchDT_F5/${FILE}.pbs
sleep 0.1
Run=$((Run+1))
done


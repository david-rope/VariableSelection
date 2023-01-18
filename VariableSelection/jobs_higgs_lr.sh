#!/bin/bash

BATCH='qsub'

INPUTDIR=/data1/David/BashR/KFoldsCV/Higgs/ScriptsLR_F3

# Where am I?
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INITJOB=1
NJOBS=$(ls $INPUTDIR | wc -l)

echo "Num of jobs: " $NJOBS

Run=1

mkdir -p $DIR/batchLR_F3/

for f in $(ls $INPUTDIR/*.R)
do
RunNum=$((Run))
FILE=lr-${RunNum}-of-${NJOBS}

cat >  $DIR/batchLR_F3/$FILE.pbs <<EOF
#!/bin/bash

#PBS -l walltime=30:00:00

Rscript ${f} 
EOF
$BATCH -o ${DIR}/batchLR_F3/ -e ${DIR}/batchLR_F3/ batchLR_F3/${FILE}.pbs
echo $BATCH ${DIR}/batchLR_F3/${FILE}.pbs
sleep 0.1
Run=$((Run+1))
done


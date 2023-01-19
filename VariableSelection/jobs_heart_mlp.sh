#!/bin/bash

BATCH='qsub'

# Check script/ directory
INPUTDIR=/Heart/ScriptsMLP_F5

# Where am I?
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INITJOB=1
NJOBS=$(ls $INPUTDIR | wc -l)

echo "Num of jobs: " $NJOBS

Run=1

mkdir -p $DIR/batchMLP_F5/

for f in $(ls $INPUTDIR/*.R)
do
RunNum=$((Run))
FILE=mlp-${RunNum}-of-${NJOBS}

cat >  $DIR/batchMLP_F5/$FILE.pbs <<EOF
#!/bin/bash

#PBS -l walltime=30:00:00

Rscript ${f} 
EOF
$BATCH -o ${DIR}/batchMLP_F5/ -e ${DIR}/batchMLP_F5 batchMLP_F5/${FILE}.pbs
echo $BATCH ${DIR}/batchMLP_F5/${FILE}.pbs
sleep 0.1
Run=$((Run+1))
done


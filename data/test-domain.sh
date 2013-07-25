#! /bin/bash
BASEDIR=$(dirname $(realpath $0))
echo BASEDIR: $BASEDIR

TIME_LIMIT=1800
MEMORY_USAGE=1000000
	
while getopts ":t:m:" opt
do
    case ${opt} in
	t) # echo limit execution time under 30 min (same as ICAPS)
	    TIME_LIMIT=${OPTARG:-$TIME_LIMIT} ;;
	
	m) # limit memory usage under 1 GB
	    MEMORY_USAGE=${OPTARG:-$MEMORY_USAGE} ;;
	
	* ) echo "unsupported option" ;;
    esac
done

shift $(($OPTIND - 1))

DOMAIN_DIR=$1
DOMAIN_DIR_LS=$(ls -v $DOMAIN_DIR)

echo $DOMAIN_DIR_LS
# exit 0

# echo "ulimit -m$MEMORY_USAGE -t$TIME_LIMIT"

ulimit -m$MEMORY_USAGE -t$TIME_LIMIT

pushd $DOMAIN_DIR
for pddl in $DOMAIN_DIR_LS
do
    echo $pddl
    if [[ (( ($pddl =~ .*\.pddl ) || ( $pddl =~ pfile.* ))
                     && (! ( $pddl =~ .*domain.*$ ))) ]]
    then
	echo PDDL: $DOMAIN_DIR/$pddl
	$BASEDIR/test-problem.sh $pddl
	case $? in
	    (0)
		echo $'\x1b[32;1m'---- computation FINISHED at $$-------------------------$'\x1b[0m' ;;
	    (1)
		echo $'\x1b[31;1m'---- computation TIMEOUT ${TIME_LIMIT} [sec.] at $$ ----$'\x1b[0m' ;
		exit 1;;
	esac
    fi
done
popd

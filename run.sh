day=$(date | grep -Eo "[0-9]{1,2}" | head -1)
if [[ $1 =~ ^[0-9]+$ ]]; then
    day=$1
    func=$2
else
    func=$1
fi

if [[ $day -lt 10 ]]; then
    dayStr="0$day"
else
    dayStr="$day"
fi

if [[ -z $func ]]; then
    func="solution"
fi

echo -e "(executable\n (public_name aoc2023)\n (name main)\n (libraries day$dayStr))" > ./bin/dune
echo "Day$dayStr.$func ()" > ./bin/main.ml
dune build && ./_build/default/bin/main.exe

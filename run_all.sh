duneFile="(executable\n (public_name aoc2023)\n (name main)\n (libraries"
mainFile=""

for day in {1..25}; do
    if [[ $day -lt 10 ]]; then
        dayStr="0$day"
    else
        dayStr="$day"
    fi

    duneFile+=" day$dayStr"
    mainFile+="Day$dayStr.solution ();\n"
done

duneFile+="))"
echo -e $duneFile > ./bin/dune
echo -e $mainFile > ./bin/main.ml
dune build --profile release && ./_build/default/bin/main.exe

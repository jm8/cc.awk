bad=0
for dir in $(find $(dirname $0)/awks/* -type d -maxdepth 0 -not -name gawk); do
    PATH="$dir:$PATH" $(dirname $0)/test.sh || bad=1
done
exit $bad

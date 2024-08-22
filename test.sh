TEST_NO=$(ls tests/*.smll | wc -l)

COUNT=0
ls tests/*.smll | while read file; do
  ./target/debug/smll run - $file
  if [ $? -eq 0 ]; then
    printf "\033[32mSuccess\033[0m\n"
    ((COUNT++))
  else
    printf "\033[31mFail\033[0m\n"
  fi
done

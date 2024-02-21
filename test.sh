
TEST_NO=`ls tests/*.sml | wc -l`

COUNT=0
ls tests/*.sml | while read file 
do 
  printf "Compiling file: %s\n" $file
  ./target/debug/smll --run $file
  if [ $? -eq 0 ]; then 
    printf "\033[32mSuccess\033[0m\n"
    COUNT=$(($COUNT + 1))
  else 
    printf "\033[31mFail\033[0m\n"
  fi
done

printf "\n%s/%s Test passed\n" $COUNT $TEST_NO


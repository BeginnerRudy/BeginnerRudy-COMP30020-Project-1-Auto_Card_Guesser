declare -a suit=("S" "H" "D" "C")
declare -a rank=("2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A")
declare -a deck=()
declare -a two_cards_time=()
declare -a three_cards_time=()
declare -a four_cards_time=()
declare -a two_guess=()
declare -a two_mark=()
declare -a three_guess=()
declare -a four_guess=()
declare -a helper=()

for i in "${rank[@]}"
do 
    for j in "${suit[@]}"
        do 
            deck+=("$i$j")
        done
done

# 2 cards
# for i in "${deck[@]}"
#     do 
#         for j in "${deck[@]}"
#             do
#                 if [ "$i" == "$j" ]; then
#                     # do nothing
#                     # echo "repeated"
#                     :
#                 else
#                     info=$( (./Proj1Test $i $j) | sed 's/[^0-9]*//g')
#                     helper=()
#                     for word in ${info[@]}
#                     do
#                         helper+=($word)
#                     done
#                     two_guess+=(${helper[-2]})
#                     two_mark+=(${helper[-1]})
#                     echo ${helper[-2]}
#                 fi
#             done
#     done

# for word in ${two_guess[@]}
# do
#     echo $word
# done

# for word in ${two_mark[@]}
# do
#     echo $word
# done

# 3-cards guess count
for i in "${deck[@]}"
    do 
        for j in "${deck[@]}"
            do
                for k in "${deck[@]}"
                do
                    if [ "$i" == "$j" ] || [ "$i" == "$k" ] || [ "$j" == "$k" ]; then
                        # do nothing
                        # echo "repeated"
                        :
                    else
                        info=$( (./Proj1Test $i $j $k) | sed 's/[^0-9]*//g')
                        helper=()
                        for word in ${info[@]}
                        do
                            helper+=($word)
                        done
                        three_guess+=(${helper[-2]})
                        echo ${helper[-2]}
                    fi
                done
            done
    done

# 4 cards
# 3-cards guess count
# for i in "${deck[@]}"
#     do 
#         for j in "${deck[@]}"
#             do
#                 for k in "${deck[@]}"
#                 do
#                     for p in "${deck[@]}"
#                     do
#                         if [ "$i" == "$j" ] || [ "$i" == "$k" ] || [ "$i" == "$p" ] || [ "$j" == "$k" ] || [ "$j" == "$p" ] || [ "$k" == "$p" ]; then
#                             # do nothing
#                             # echo "repeated"
#                             :
#                         else
#                             info=$( (./Proj1Test $i $j $k $p) | sed 's/[^0-9]*//g')
#                             helper=()
#                             for word in ${info[@]}
#                             do
#                                 helper+=($word)
#                             done
#                             # four_guess+=(${helper[-2]})
#                             echo ${helper[-2]}
#                         fi
#                     done
#                 done
#             done
    # done

# 2 cards
# for i in "${deck[@]}"
#     do 
#         for j in "${deck[@]}"
#             do
#                 two_cards_time+=$(  TIMEFORMAT='%lU';  time (./Proj1Test $i $j $k)  2>&1  1>/dev/null )
#             done
#     done

# for i in "${two_cards_time[@]}"
#     do 
#         echo $i
#     done
# 3 cards
# for i in "${deck[@]}"
#     do 
#         for j in "${deck[@]}"
#             do
#                 for k in "${deck[@]}"
#                     do
#                         echo $(  TIMEFORMAT='%lU';  time (./Proj1Test $i $j $k)  2>&1  1>/dev/null )
#                     done
#             done
#     done

# 4 cards
# for i in "${deck[@]}"
#     do 
#         for j in "${deck[@]}"
#             do
#                 for k in "${deck[@]}"
#                     do
#                         for p in "${deck[@]}"
#                             do
#                                 echo $(  TIMEFORMAT='%lU';  time (./Proj1Test $i $j $k $p)  2>&1  1>/dev/null )
#                             done
#                     done
#             done
#     done
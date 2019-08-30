def average_guess_count(filename):
    f= open(filename,"r")
    count = []
    for line in f:
        count.append(int(line))
    f.close()

    print(filename + ": " + str(sum(count)) + "/" + str(len(count)) + " = " + str(sum(count)/len(count)))

average_guess_count("two_guess_count.text")
average_guess_count("three_guess_count.text")
average_guess_count("four_guess_count.text")
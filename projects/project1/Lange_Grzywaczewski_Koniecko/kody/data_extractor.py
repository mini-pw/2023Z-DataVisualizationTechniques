lines = open('data.txt').read().split('\n')
count = 0
for line in lines:
    if line != '':
        print(line)
        count +=1

print(count)
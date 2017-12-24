import csv

contents = list()
hamming = list()
haccp = list()
name = list()
product = input("유사 제품을 입력해 주세요 : ")
user = list()

with open("ingredient_result.txt", "r") as f:
	read = f.read()

ingredients = read.replace('"', "").split(",")

with open('test_matrix.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        contents.append(row)

with open('name_lookup.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        name.extend(row)

with open('haccp.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        haccp.extend(row)

name = list(filter(None, name))
user.extend(contents[name.index(product)])  

ingredient = ' '
while ingredient != 'x' :
	try:
		ingredient = input("드시면 안되는 성분 명을 입력해주세요 더이상 없다면 x : ")
		if ingredient != 'x' :
			user[ingredients.index(ingredient)] = '-1'
	except ValueError:
		print('값이 적절하지 않습니다.')      

for i, row in enumerate(contents):
	res = 0
	for j , col in enumerate(row):
		if user[j] == '-1' and contents[i][j] == '1' :
			res = 200
			break
		if user[j] != contents[i][j] :
			res = res + 1
	if haccp[i] == 1 :
		res = res - 5
	hamming.append(res)

hamming[name.index(product)] = 194

for i in range(0, 5) :
	index = hamming.index(min(hamming))
	hamming[index] = 200
	print (name[index])
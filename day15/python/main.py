
# python recap 
print("hello to python !")

m = [ [1,2,3],[4,5,6],[7,8,9] ]

print(m)
print(m[0][0])
print(m[2][2])

id = 0 

def goblin():
    global id
    id = id + 1
    return ['goblin',200,id]

def elf():
    global id
    id = id + 1
    return ['elf',200,id]

print(elf())
print(elf())
print(goblin())
print(goblin())

print('elf' == 'goblin')
print('elf' == 'elf')

# capital True False booleans


#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#   
#######




from sys import stdin

def load_commands():
    root = ("/", {})
    path = [root]
    for line in stdin:
        cmd = line.strip().split(" ")
        if len(cmd) == 0:
            continue
        if cmd[0] == "$" and cmd[1] == "ls":
            pass
        elif cmd[0] == "$" and cmd[1] == "cd":
            dest = cmd[2]
            if dest == "/":
                path = [root]
            elif dest == "..":
                path.pop()
            else:
                # dest is an explicit directory
                parent = path[-1]  # last element
                if dest not in parent[1]:
                    parent[1][dest] = {}
                path += [(dest, parent[1][dest])]
        else:
            cwd = path[-1]
            arg = cmd[1]
            if cmd[0] == "dir":
                if arg not in cwd[1]:
                    cwd[1][arg] = {}
            else:
                cwd[1][arg] = int(cmd[0])

    return {"/":root[1]}

##debugging
def pr_tree(dir, depth=0):
    for k in dir.keys():
        print(depth * " ", "-", k, end='')
        if type(dir[k]) is int:
            print(f" (file, size={dir[k]})")
        else:
            print(" (dir)")
            pr_tree(dir[k], depth=depth+1)


def size_dir(dir, res):
    total = 0
    for k in dir.keys():
        v = dir[k]
        if type(v) is int:
            dsize = v
        else:
            dsize = size_dir(v, res)
            res += [(k, dsize)]
        total += dsize
    return total

def solve_part1():
    d = load_commands()
    res = []
    root_size = size_dir(d, res)
    total = 0
    for (_, s) in res:
        if s <= 100000:
            total+= s
    print(total)

def solve_part2():
    d = load_commands()
    res = []
    root_size = size_dir(d, res)
    available = 70000000 - root_size
    needed = 30000000 - available
    if needed <= 0:
        print ("Impossible", available, root_size)
        return
    min_dir = 30000000
    for (_, s) in res:
        if s >= needed and s < min_dir:
            min_dir = s
    print(min_dir)
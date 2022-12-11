import importlib

__SOLUTIONS = {}


def get(name):
    if name in __SOLUTIONS:
        return __SOLUTIONS[name]
    return None


for mod in range(1, 26):
    try:
        mod_name = f"s{mod:02}"
        m = importlib.import_module(mod_name)
        for i in [1, 2]:
            f_name = f"solve_part{i}"
            sol_name = f"{mod:02}_part{i}"
            try:
                f = getattr(m, f_name)
                __SOLUTIONS[sol_name] = f
            except AttributeError:
                print(f"Warning: missing function {f_name} in file {mod_name}.py")
    except ModuleNotFoundError:
        pass
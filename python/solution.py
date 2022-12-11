import importlib

__SOLUTIONS = {}


def register(name, f):
    __SOLUTIONS[name] = f


def get(name):
    if name in __SOLUTIONS:
        return __SOLUTIONS[name]
    return None


for mod in [f"s{x:02}" for x in range(1, 26)]:
    try:
        importlib.import_module(mod)
    except ModuleNotFoundError:
        pass
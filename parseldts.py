#!/usr/bin/python

import sys, os.path, re

groups = {
    "sorts":[],
    "functions":[],
    "predicates":[]
}

current_file = None

def analyze_sorts(seq):
    for n,line in seq:
        line = line.strip()
        if line.startswith("//"): continue
        elif line.startswith("}"): break

        for item in line.split(";"):
            groups["sorts"].append((item, "Sort", f"{current_file}:{n}"))

def analyze_functions(seq, group="functions"):
    for n,line in seq:
        line = line.strip()
        if line.startswith("//") or line == '': continue
        elif line.startswith("}"): break
        else:
            for item in line.split(";"):
                try:
                    items = re.split("[ (]", item)
                    name = items[0] if group != "functions" else items[1]
                    if name != "": 
                        groups[group].append((name, item, f"{current_file}:{n}"))
                except IndexError as e:
                    #print(f"Error with line '{item}'")
                    pass

            
def analyze(seq):
    seq = enumerate(seq)
    for num, line in seq:
        line = line.strip()
        if line.startswith("\\sorts"):
            analyze_sorts(seq)
        if line.startswith("\\functions"):
            analyze_functions(seq)
        if line.startswith("\\predicates"):
            analyze_functions(seq, "predicates")

def printSexpr():
    print("'(")
    for k,v in groups.items():
        print(f"(\"{k}\" . (")
        for a in v:
            print(f"(\"{a[0]}\" \"{a[1]}\" \"{a[2]}\")")
        print(")")
    print(")")

            
for f in sys.argv[1:]:
    with open(f) as fh:
        current_file = os.path.basename(f)
        analyze(fh)

printSexpr()

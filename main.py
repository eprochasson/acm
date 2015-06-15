from case import *

def load_cases(input_file = 'input.txt'):
    with open(input_file) as f:
        cases = []
        first_line = True
        n_machines = 0
        balance = 0
        days = 0
        machines = []
        for l in f:
            line = map(lambda x: int(x), l.split())
            if len(line) == 3:
                # If not the first line, store what we accumulated already
                if not first_line:
                    cases.append(Case(n_machines, balance, days, machines))
                    # Last line: spit out the result of parsing
                    if line[0] == 0 and line[1] == 0 and line[2] == 0:
                        return cases
                else:
                    first_line = False

                # Start accumulating a new case
                n_machines = line[0]
                balance = line[1]
                days = line[2]
                machines = []
            else:
                machines.append(line)

cases = load_cases('input.txt')
for idx, c in enumerate(cases):
    print "Case {}: {} ({} Iterations)".format(idx+1, c.solve(), c.total_iteration)
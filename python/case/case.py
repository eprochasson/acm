class Case:
    def __init__(self, n_machines, balance, days, machines):
        self.n_machines = n_machines
        self.balance = balance
        self.days = days
        self.machines = []

        id = 0
        for m in machines:
            # A machine ID is it's position in the self.machines array.
            self.machines.append({ 'id': id, 'day': m[0], 'cost': m[1], 'sell': m[2], 'profit': m[3]})
            id += 1

    def solve(self):
        self.total_iteration = 0
        return self._solve(
            day = 1,
            balance = self.balance,
            inventory=None,
            available_machines = sorted(self.machines, key=lambda x: x['day'])
        )

    def _solve(self, day = 1, balance = 0, inventory = None, available_machines = None):
        self.total_iteration += 1
        # Stop condition: no more days
        if day > self.days:
            if inventory != None:
                return balance + inventory['sell']
            else:
                return balance

        # Stop condition: no more machine to buy, just cash profit until the end of the period
        if available_machines == None or len(available_machines) == 0:
            if inventory != None:
                # Cash daily profit, sell on the last day
                return balance + ((self.days - day + 1) * inventory['profit']) + inventory['sell']
            else:
                return balance

        # Each day, 2 options:
        # - if a machine is available for buy, we can sell/buy, then skip to the next day
        # OR
        # - we can cash out for the day and skip to the next one

        # The list of machine we could buy:
        to_buy = self._get_available_machines(day, balance, inventory, available_machines)

        # If we were to buy, how much we'd make by selling the current inventory
        if inventory != None:
            sell_price = inventory['sell']
        else:
            sell_price = 0

        # If we don't buy, how much profit we'd make that day
        if inventory != None:
            daily_profit = inventory['profit']
        else:
            daily_profit = 0


        # Generate all branches where we buy
        branches = []

        for b in to_buy:
            branches.append({
                'day': day + 1,
                'inventory': b,
                'balance': balance + sell_price - b['cost'],
                'available_machines' : [a for a in available_machines if a['id'] != b['id'] and a['day'] > day ]
                                                                                    # Remove the machine
                                                                                    # for future iteration
            })

        # Select the best branch
        return max(
            map(lambda x: self._solve(** x), branches) + # Test all buying branch
            [self._solve(    # Or skip to the next day
                day+1,
                balance + daily_profit,
                inventory,
                [a for a in available_machines if a['day'] > day ])
            ])


    # Extract from the list of machines the one that 1. are available that day 2. that we can afford
    def _get_available_machines(self, day, balance, inventory, machines):
        if inventory != None:
            # How much money we can gather if we sell our existing inventory
            potential_balance = balance + inventory['sell']
        else:
            potential_balance = balance

        # Search for one or more machine available that day
        available = []
        for m in machines:
            if m['day'] == day and m['cost'] <= potential_balance:
                available.append(m)
        return available

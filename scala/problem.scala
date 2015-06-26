package problem

class Problem(balance: Int, days: Int, machines: List[Array[Int]]) {
  class Machine(val day: Int, val cost: Int, val sell: Int, val profit: Int)

  def solve(
              day: Int = 1,
              balance: Int = this.balance,
              inventory: Machine = null,
              availableMachines: List[Machine] = this.machines.map(m => new Machine(m(0), m(1), m(2), m(3)))): Int =
    (day, balance, inventory, availableMachines) match {
      case (this.days, b, null, _) => // Last day, no inventory
        b
      case (this.days, b, i, _) => // Last day, something in the inventory. Sell it.
        b + i.sell + i.profit
      case (_, b, null, Nil) => // No machine left to buy, no inventory
        b
      case (d, b, i, Nil) => // No machine left to buy, some inventory
        b + ((this.days - d + 1)* i.profit) + i.sell
      case _ =>
        // All the machines we can buy (time and money)
        val buy_branches = this.availableMachines(day, balance, inventory, availableMachines)

        // Calculate the branch where we keep the current machine
        val not_buy_branch_balance: Int = this.solve( // Branch: we don't buy a new machine and keep going with this one
          day+1, // Move one day
          balance + (if (inventory == null) 0 else inventory.profit), // Cash potential profit
          inventory, // Don't change the inventory
          availableMachines.filter(m => m.day > day)) // Filter the expired machines

        // Compute and compare all branches
        (not_buy_branch_balance :: buy_branches.map(m => this.solve(
          day+1,
          balance - m.cost + (if (inventory == null) 0 else inventory.sell),
          m,
          availableMachines.filter(i => (i != m) && (i.day > day))))).max
  }

  def availableMachines(day: Int, balance: Int, inventory: Machine = null, machines: List[Machine]): List[Machine] = {
    machines.filter(m => (m.day == day) && (m.cost <= (if (inventory == null) balance else balance + inventory.sell)))
  }
}

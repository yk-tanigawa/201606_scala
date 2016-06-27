class BankAccount {
  private var bal: Int = 0
  def balance = bal
  def deposit(amount: Int) {
    require(amount > 0)
    bal += amount
  }
  def withdraw(amount: Int): Boolean =
    if(amount > bal) false
    else {    
      bal -= amount
      true
    }
}

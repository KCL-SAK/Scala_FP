// Part 1 about the 3n+1 conjecture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

//def collatz(n: Long) : Long = ...
def collatz(x : Long) : Long = {
  //val n = 1
  //if(x > 0)
    if(x == 1) 0
    else if(x%2 == 0) 1 + collatz(x/2)
    else 1 + collatz(3*x+1)
}
//println("hello " + collatz(9))


//(2)  Complete the collatz_max function below. It should
//     calculate how many steps are needed for each number 
//     from 1 up to a bound and then calculate the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. Again, you should expect bounds in the range of 1
//     up to 1 Million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

//def collatz_max(bnd: Long) : (Long, Long) = ...


def collatz_max(max : Int) : (Long, Long) = {
  val elements = (1 to max).toList
  val steps = (1 to max).map(x => collatz(x))
  val elementsIndex = steps.indexOf(steps.max)
  (steps.max, elements(elementsIndex))
}
//println("hello " + collatz_max(10))

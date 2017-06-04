package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  val set1 = union(singletonSet(2),singletonSet(3))
  println(forall(set1,x => x> 1))
  
  println(exists(set1,x => x> 2))
  
  val set2 = map(set1,x=>x+4)
  
  printSet(set2)
  
  def set3(x: Int):Boolean = {
    x==1|x==3|x==4|x==5|x==7|x==1000
  }
  
  def set4(x: Int):Boolean = {
    x==1|x==3|x==4|x==5|x==7|x==1000
  }
}

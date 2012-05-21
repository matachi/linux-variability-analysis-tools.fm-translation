package gsd.linux.cnf

trait ImplBuilderStats extends ImplBuilder {

  var numImplCalls = 0
  private var _numImpl = new collection.mutable.ListBuffer[(Int,Int)]
  def numImpl = (Set() ++ _numImpl).size

  override def implication(v1: Int, v2: Int) = {
    numImplCalls += 1
    val isImpl = super.implication(v1, v2)
    if (isImpl)  _numImpl += Tuple2(v1, v2)
    isImpl
  }

}
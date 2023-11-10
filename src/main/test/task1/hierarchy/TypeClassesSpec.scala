package task1.hierarchy;
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import task1._
import task1.hierarchy.TypeClasses.{treeApplicative, treeApply, treeFlatMap, treeFunctor}

class TypeClassesSpec extends AnyFlatSpec with Matchers {
  val singleLeaf: Leaf[Int] = Leaf(1)
  val simpleBranch: Branch[Int] = Branch(Leaf(1), Leaf(2))
  val complexTree: Branch[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  "Functor[Tree]" should "correctly map a function over a Tree" in {
    val result = treeFunctor.map(complexTree)(_ * 2)
    result shouldBe Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
  }

  "Apply[Tree]" should "correctly apply a Tree of functions to a Tree of values" in {
    val treeOfFunctions = Branch(Leaf((x: Int) => x * 2), Leaf((x: Int) => x + 3))
    val result = treeApply.ap(treeOfFunctions)(simpleBranch)
    result shouldBe Branch(Leaf(2), Leaf(5))
  }

  "Applicative[Tree]" should "lift a value into a Leaf" in {
    val result = treeApplicative.pure(5)
    result shouldBe Leaf(5)
  }

  "FlatMap[Tree]" should "correctly flatMap a function over a Tree" in {
    val result = treeFlatMap.flatMap(simpleBranch)(x => Branch(Leaf(x), Leaf(x + 1)))
    result shouldBe Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(2), Leaf(3)))
  }
}

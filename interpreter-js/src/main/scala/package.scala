package ltbs.uniform

package object prototype {

  implicit class RichList[ELEM](inner: List[ELEM]) {
    def replace(ordinal: Int, elem: ELEM): List[ELEM] = 
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ {elem :: inner.drop(ordinal + 1)}
      else
        throw new IndexOutOfBoundsException

    def delete(ordinal: Int): List[ELEM] =
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ inner.drop(ordinal + 1)
      else
        throw new IndexOutOfBoundsException
  }
  
}

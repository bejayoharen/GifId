/**
  * Created by bjorn on 5/1/17.
  */
case class SignatureAtom(a:Double, y:Double, u:Double, v:Double ) {
  def quantize(allowableError: Double, v:Double): List[Byte] = { //FIXME: this should be static and private. How do we test private?
    val l = 16 - 1

    var ret = List[Byte]()

    val vv = if(v > 1.0) { 1.0 } else { if(v < 0.0) 0.0 else v }
    val b = ( vv * l + .5 ).toByte

    // was it close to being quantized to another value? If so, add that to the list.
    val bu = (vv * l + .5 + allowableError ).toInt
    val bd = (vv * l + .5 - allowableError ).toInt

    if( bu != b && bu <= l )
      ret = bu.toByte :: ret
    if( bd != b && bd >= 0 )
      ret = bd.toByte :: ret

    // add the main quantization to the list
    ret = b :: ret

    ret
  }
  private def pack4Bytes( v: Vector[Byte] ): Array[Byte] = {
    val b1: Byte = v(0)
    var b2: Byte = v(1)
    var b3: Byte = v(2)
    var b4: Byte = v(3)

    // pack into byte array:
    return Array(
      ((b1 << 4) | b2).toByte,
      ((b3 << 4) | b4).toByte
    )
  }
  def quantizedBytesAlternate(allowableError:Double): List[Array[Byte]] = {
    var b = List[List[Byte]]()
    b = quantize(allowableError,v) :: b
    b = quantize(allowableError,u) :: b
    b = quantize(allowableError,y) :: b
    b = quantize(allowableError,a) :: b

    b.foldLeft[List[Vector[Byte]]]( List[Vector[Byte]]() ) { (a,b) =>
      var ret = List[Vector[Byte]]()
      //println( a, b )
      a match {
        case List() => {
          for( j <- b ) {
            //println( ":", j )
            var cp = Vector[Byte]()
            cp = cp :+ j
            ret = cp :: ret
          }
        }
        case _ => {
          for( i <- a ; j <- b ) {
            //println( i, j )
            var cp: Vector[Byte] = i map(identity)
            cp = cp :+ j
            ret = cp :: ret
          }
        }
      }

      ret
    }.map( a => {
      //println( "converting a:", a, a.length )
      //println( "converting a:", pack4Bytes( a )(0), pack4Bytes( a )(1) )
      pack4Bytes( a )
    } ).reverse
  }
}

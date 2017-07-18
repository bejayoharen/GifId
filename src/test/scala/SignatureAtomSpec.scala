import org.scalatest._
import org.scalatest.FlatSpec

class SignatureAtomSpec extends FlatSpec {
  "A Signature Atom" must "give a single quantization when allowable error is zero" in {
    val sa = new SignatureAtom(.5,.5,.5,.5)
    assert( sa.quantize(0, 0).length ===  1 )
    assert( sa.quantize(0,.1).length ===  1 )
    assert( sa.quantize(0,.5).length ===  1 )
    assert( sa.quantize(0, 1).length ===  1 )
  }
  it must "quantize to 4 bits" in {
    val sa = new SignatureAtom(.5,.5,.5,.5)
    assert( sa.quantize(0,.5)(0) < 16 )
    assert( sa.quantize(0, 0)(0) < 16 )
    assert( sa.quantize(0, 1)(0) < 16 )
    assert( sa.quantize(0,-1)(0) < 16 )
    assert( sa.quantize(0, 2)(0) < 16 )
  }
  it must "quantize correctly when allowableError is zero" in {
    val sa = new SignatureAtom(.5,.5,.5,.5)

    val q = 1.0/15.0  // quantum
    val h = 1.0/30.0  // half-quantum
    val s = 1.0/100.0 // something small
    for( a <- 0 to 15 ) {
      val ap = (if( a == 15 ) 15 else a+1).toByte
      val am = (if( a == 0  )  0 else a-1).toByte

      val l = sa.quantize(0,a*q)
      assert( l.length === 1 )
      assert( l(0) === a )
      val lh = sa.quantize(0,a*q+h)
      assert( lh.length === 1 )
      assert( lh(0) === ap )
      val ls = sa.quantize(0,a*q+s)
      assert( ls.length === 1 )
      assert( ls(0) === a )
      val lhs = sa.quantize(0,a*q+s+h)
      assert( lhs.length === 1 )
      assert( lhs(0) === ap )
      val lms = sa.quantize(0,a*q-s)
      assert( lms.length === 1 )
      assert( lms(0) === a )
    }
  }
  it can "give one or two quantizations" in {
    val sa = new SignatureAtom(.5,.5,.5,.5)

    val q = 1.0/15.0  // quantum
    val h = 1.0/30.0  // half-quantum
    val s = 1.0/100.0 // something small
    val e = 1.0/5.0  // error value
    for( a <- 1 to 14 ) {
      val l1 = sa.quantize(e,a*q)
      assert( l1.length === 1 )
      assert( l1(0) === a )

      val l2 = sa.quantize(e,a*q+h+s)
      assert( l2.length === 2 )
      assert( l2(0) === a+1 )
      assert( l2(1) === a )
      val l3 = sa.quantize(e,a*q+h-s)
      assert( l3.length === 2 )
      assert( l3(0) === a )
      assert( l3(1) === a+1 )
    }
  }

  "Quantize Bytes Alternate" must "give the correct, single response of length 2 bytes when allowable error is zero" in {
    val q = 1.0/15.0  // quantum
    val sa = new SignatureAtom(q, q, q, q)
    val l = sa.quantizedBytesAlternate(0)
    assert(l.length === 1)
    assert(l(0).length === 2)
    assert(l(0)(0) === 17)
    assert(l(0)(1) === 17)

    val sa2 = new SignatureAtom(2*q, 3*q, 4*q, 5*q)
    val l2 = sa2.quantizedBytesAlternate(0)
    assert(l2.length === 1)
    assert(l2(0).length === 2)
    assert(l2(0)(0) === 35)
    assert(l2(0)(1) === 69)

    val sa3 = new SignatureAtom(12*q, 13*q, 15*q, 17*q)
    val l3 = sa3.quantizedBytesAlternate(0)
    assert(l3.length === 1)
    assert(l3(0).length === 2)
    assert(l3(0)(0) === 205.toByte )
    assert(l3(0)(1) === 255.toByte )
  }

  it must "give the correct multiple responses when allowable error is larger than zero" in {
    val q = 1.0/15.0  // quantum
    val h = 1.0/30.0  // half-quantum
    val s = 1.0/100.0 // something small
    val e = 1.0/5.0  // error value

    var sa = new SignatureAtom(2*q+h, 3*q, 4*q, 5*q)
    var l = sa.quantizedBytesAlternate(e)
    assert( l.length === 2 )

    assert(l(0)(0) === (2 << 4 | 3).toByte )
    assert(l(0)(1) === (4 << 4 | 5).toByte )
    assert(l(1)(0) === (3 << 4 | 3).toByte )
    assert(l(1)(1) === (4 << 4 | 5).toByte )


    sa = new SignatureAtom(2*q, 3*q+h+s, 4*q, 5*q+h-s)
    l = sa.quantizedBytesAlternate(e)
    assert( l.length === 4 )

    assert(l(0)(0) === (2 << 4 | 4).toByte )
    assert(l(0)(1) === (4 << 4 | 5).toByte )
    assert(l(1)(0) === (2 << 4 | 4).toByte )
    assert(l(1)(1) === (4 << 4 | 6).toByte )
    assert(l(2)(0) === (2 << 4 | 3).toByte )
    assert(l(2)(1) === (4 << 4 | 5).toByte )
    assert(l(3)(0) === (2 << 4 | 3).toByte )
    assert(l(3)(1) === (4 << 4 | 6).toByte )
  }
}
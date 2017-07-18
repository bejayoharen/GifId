import java.awt.image.BufferedImage

import org.scalatest.FlatSpec

class TilingSpec extends FlatSpec {

  private def q8(v:Double): Int = {
    val vv = if( v > 1 ) 1 else ( if( v < 0 ) 0 else v )
    ( v * 255 + .5 ).toInt
  }
  private def testColorAYUV( a:Double, y:Double, u:Double, v:Double ): Int = {
    val r = 1 * y + 0 * u + 1.28033 * v
    val g = 1 * y + -0.21482 * u + -0.38059 * v
    val b = 1 * y + 2.12798 * u + 0 * v
    val rgb = ( q8(a) << 24 ) | ( q8(r) << 16 ) | ( q8(g) << 8 ) | ( q8(b) << 0 )

    rgb
  }
  private def testImageWithFourColors( a:Int, b:Int, c:Int, d:Int ): BufferedImage = {
    val bi = new BufferedImage(10,10,BufferedImage.TYPE_INT_ARGB)

    for( i <- 0 until bi.getHeight/2; j <- 0 until bi.getWidth/2 ) {
      bi.setRGB(i,j,a)
    }
    for( i <- 0 until bi.getHeight/2; j <- bi.getWidth/2 until bi.getWidth ) {
      bi.setRGB(i,j,b)
    }
    for( i <- bi.getHeight/2 until bi.getHeight; j <- 0 until bi.getWidth/2 ) {
      bi.setRGB(i,j,c)
    }
    for( i <- bi.getHeight/2 until bi.getHeight; j <- bi.getWidth/2 until bi.getWidth ) {
      bi.setRGB(i,j,d)
    }

    bi
  }

  "A Tiling's Signatures" must "give a single signature when allowable error is zero" in {
    val q = 1.0/15.0  // quantum
    val tc = testColorAYUV( q, q, q, q )
    val bi = testImageWithFourColors( tc, tc, tc, tc )

    val st = StandardTiling(bi,2,2)

    val tiles = st.getTiles()
    assert( tiles.length === 4 )

    val sigs = st.getSignatures(bi,0)
    assert( sigs.length === 1 )

    assert( sigs(0).length === 4 * 2 )
  }
  it can "give multile quantization results" in {
    val q = 1.0/15.0  // quantum
    val h = 1.0/30.0  // half
    val e = 1.0/50.0 // error

    val tc0 = testColorAYUV( q, q, q, q )
    val tc1 = testColorAYUV( q, q+h, q, q )

    var bi = testImageWithFourColors( tc0, tc0, tc0, tc0 )
    var st = StandardTiling(bi,2,2)
    var tiles = st.getTiles()
    assert( tiles.length === 4 )
    var sigs = st.getSignatures(bi,e)
    assert( sigs.length === 1 )

    bi = testImageWithFourColors( tc0, tc1, tc0, tc0 )
    st = StandardTiling(bi,2,2)
    tiles = st.getTiles()
    assert( tiles.length === 4 )
    sigs = st.getSignatures(bi,e)
    assert( sigs.length === 2 )

    bi = testImageWithFourColors( tc0, tc1, tc0, tc1 )
    st = StandardTiling(bi,2,2)
    tiles = st.getTiles()
    assert( tiles.length === 4 )
    sigs = st.getSignatures(bi,e)
    assert( sigs.length === 4 )

    //println( st.getSignaturesBase64(bi,e).foldLeft(""){ _  + "=" + _ } )
  }
}
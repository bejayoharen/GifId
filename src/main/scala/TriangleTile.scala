import java.awt.image.BufferedImage

/**
  * Similar to standard tiling, but applies a triangle window to each tile.
  * May be modified to add some additional tiles to the middle to solve the issue of missing data.
  *
  * Created by bjorn on 5/4/17.
  */
case class TriangleTiling(bufferedImage: BufferedImage, xTiles: Int, yTiles: Int) extends Tiling {
  def getTiles(): List[Tile] = {
    var ret = List[Tile]()
    // FIXME: may want to skip bottom right where there's likely to be a watermark.
    for( i <- 0 until yTiles ; j <- 0 until xTiles ) {
      var w = bufferedImage.getWidth / xTiles
      var x = bufferedImage.getMinX + j * bufferedImage.getWidth / xTiles
      var h = bufferedImage.getHeight / yTiles
      var y = bufferedImage.getMinY + i * bufferedImage.getHeight / yTiles

      //println( x, w, y, h, "\n", bufferedImage.getMinX, bufferedImage.getWidth, bufferedImage.getMinY, bufferedImage.getHeight )

      ret = TriangleTile( x, w, y, h ) :: ret
    }
    ret
  }
}

case class TriangleTile(x: Int, w: Int, y: Int, h: Int) extends Tile {
  def evaluate(bufferedImage: BufferedImage): SignatureAtom = {
    var aa = 0.0
    var ra = 0.0
    var ga = 0.0
    var ba = 0.0
    var tot = 0.0
    for( j <- y until y+h ; i <- x until x+w ) {
      var jj: Double = (j - y).toDouble
      jj = if( jj < h / 2.0 ) {
        jj / ( h / 2.0 )
      } else {
        2 * ( 1 - jj / h )
      }
      var ii: Double = (i - x).toDouble
      ii = if( ii < w / 2.0 ) {
        ii / ( w / 2.0 )
      } else {
        2 * ( 1 - ii / h )
      }

      var f = jj * ii - .05
      f = if( f > 1 ) 1 else f
      f = if( f < 0 ) 0 else f

      val argb = bufferedImage.getRGB(i, j) //FIXME: wildly inefficient!
      val a = ( argb >> 24 ) & 0xff
      val r = ( argb >> 16 ) & 0xff
      val g = ( argb >>  8 ) & 0xff
      val b = ( argb >>  0 ) & 0xff
      aa += a.toDouble * f
      ra += r.toDouble * f
      ga += g.toDouble * f
      ba += b.toDouble * f
      tot += f
    }
    aa /= tot
    ra /= tot
    ga /= tot
    ba /= tot

    val yc = 0.2126 * ra + 0.7152 * ga + 0.0722 * ba
    val uc = -0.09991 * ra + -0.33609 * ga + 0.436 * ba
    val vc = 0.615 *ra + -0.55861 * ga + -0.5639 * ba
    return SignatureAtom( aa/255.0, yc/255.0, uc/255.0, vc/255.0 )
  }
}
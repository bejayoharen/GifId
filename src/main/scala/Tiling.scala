import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream

import sun.misc.BASE64Encoder

import scala.collection.mutable

/**
  * Created by bjorn on 4/27/17.
  */

trait Tiling {
  def getTiles(): List[Tile]
  def getSignatures(bi: BufferedImage, allowableError:Double): Seq[Array[Byte]] = {
    var c = mutable.MutableList[List[Array[Byte]]]()
    for( t <- getTiles ) {
      val atom = t.evaluate(bi)
      val cc = atom.quantizedBytesAlternate(allowableError)
      c += cc
    }
    c.foldLeft( mutable.MutableList[ByteArrayOutputStream]() ) { (acc, atoms) =>
      if( acc.length == 0 ) {
        for( atom <- atoms ) {
          val baos = new ByteArrayOutputStream()
          baos.write(atom)
          acc += baos
        }
        acc
      } else if( atoms.length == 1 ) {
        for( a <- acc ) {
          a.write(atoms(0))
        }
        acc
      } else {
        var ac2 = mutable.MutableList[ByteArrayOutputStream]()
        for( a <- acc ; atom <- atoms ) {
          val b = new ByteArrayOutputStream()
          b.write( a.toByteArray )
          b.write( atom )
          ac2 += b
        }
        ac2
      }
    }.map{ (a:ByteArrayOutputStream) => a.toByteArray }
  }
  def getSignaturesBase64(bi: BufferedImage, allowableError:Double): Seq[String] = {
    val enc = new BASE64Encoder()
    getSignatures(bi, allowableError).map(a => {
      var byteBuffer = new ByteArrayOutputStream()
      byteBuffer.write( a, 0, a.length )
      enc.encode(byteBuffer.toByteArray())
        .replace("\n", "")
        .replace("\r", "")
    })
  }
}

trait Tile {
  def evaluate(bufferedImage: BufferedImage): SignatureAtom
}

case class StandardTiling(bufferedImage: BufferedImage, xTiles: Int, yTiles: Int) extends Tiling {
  def getTiles(): List[Tile] = {
    var ret = List[Tile]()
    // FIXME: may want to skip bottom right where there's likely to be a watermark.
    for( i <- 0 until yTiles ; j <- 0 until xTiles ) {
      var w = bufferedImage.getWidth / xTiles
      var x = bufferedImage.getMinX + j * bufferedImage.getWidth / xTiles
      var h = bufferedImage.getHeight / yTiles
      var y = bufferedImage.getMinY + i * bufferedImage.getHeight / yTiles

      //println( x, w, y, h, "\n", bufferedImage.getMinX, bufferedImage.getWidth, bufferedImage.getMinY, bufferedImage.getHeight )

      ret = StandardTile( x, w, y, h ) :: ret
    }
    ret
  }
}

case class StandardTile(x: Int, w: Int, y: Int, h: Int) extends Tile {
  def evaluate(bufferedImage: BufferedImage): SignatureAtom = {
    var aa = 0.0
    var ra = 0.0
    var ga = 0.0
    var ba = 0.0
    for( j <- y until y+h ; i <- x until x+w ) {
      val argb = bufferedImage.getRGB(i, j) //FIXME: wildly inefficient!
      val a = ( argb >> 24 ) & 0xff
      val r = ( argb >> 16 ) & 0xff
      val g = ( argb >>  8 ) & 0xff
      val b = ( argb >>  0 ) & 0xff
      aa += a.toDouble
      ra += r.toDouble
      ga += g.toDouble
      ba += b.toDouble
    }
    aa /= w * h
    ra /= w * h
    ga /= w * h
    ba /= w * h

    val yc = 0.2126 * ra + 0.7152 * ga + 0.0722 * ba
    val uc = -0.09991 * ra + -0.33609 * ga + 0.436 * ba
    val vc = 0.615 *ra + -0.55861 * ga + -0.5639 * ba
    return SignatureAtom( aa/255.0, yc/255.0, uc/255.0, vc/255.0 )
  }
}

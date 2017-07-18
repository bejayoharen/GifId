import java.io._

import scala.collection.JavaConverters._
import scala.collection.mutable


object Main {
  def main(args: Array[String]): Unit = {
    val d: String = System.getProperty("user.dir") + "/assets" + "/testimages"
    val files = List( "01-01.gif", "01-02.gif", "01-03.gif", "01-04.gif", "01-05.gif",
      "02-01.gif", "02-02.gif",
      "03-01.gif", "03-02.gif",
      "04-01.gif", "04-02.gif",
      "05-01X.gif", "05-02X.gif", "05-03X.gif", "05-04X.gif" )
    val filePaths = files.map( a => ( d + "/" + a ) )

    val map = mutable.HashMap[String,GifInfo]()

    for( fp <- filePaths ) {
      println( s"--------------- ${fp}" )
      val res = fingerprintPath(fp)
      if( res.isLeft ) {
        println( res.left )
      } else {
        val gi = res.right.get
        val matches = gi.allSignatures.foldLeft(mutable.Set[GifInfo]()) { (a,b) =>
          map.get(b) match {
            case None => a
            case gi: Option[GifInfo] => a.add(gi.get) ; a
          }
        }
        for( m <- matches ) {
          println( "   Match: " + m.id + " (" + (100*gi.matchInfo(m).percentMatch) + "%)" )
        }
        if( matches.headOption.isEmpty ) {
          println( "   No Match" )
        }
        gi.allSignatures.foreach{ s =>
          if( map.get(s) == None ) {
            map.put( s, gi )
          }
        }
      }
    }
  }

  def fingerprintPath( path: String ): Either[Exception,GifInfo] = {
    var fis: InputStream = null
    try {
      // open the file
      fis = new BufferedInputStream(new FileInputStream(path))
      val fs = ImageFrame.readAnimatedGif( fis )
      Right( GifInfo( path, fs ) )
    } catch {
      case e: Exception => Left(e)
    } finally {
      if( fis != null ) {
        fis.close()
      }
    }
  }
}
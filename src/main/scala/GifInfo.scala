import scala.collection.mutable
import scala.collection.immutable

/**
  * Created by bjorn on 5/2/17.
  */

case class GifInfo( id:String, allSignatures: immutable.Set[String], frames: Array[immutable.Set[String]], totalDelay: Int ) {
  def matchInfo( gi: GifInfo ): MatchInfo = {
    var hits = 0
    for( i <- frames ) {
      val found = i.foldLeft( false ) { (acc,c) => acc | gi.allSignatures.contains(c) }
      hits = if( found ) hits+1 else hits
    }
    MatchInfo( hits.toDouble / hits.toDouble )
  }
}

case class MatchInfo( percentMatch: Double )

object GifInfo {
  def apply( id: String, imageFrames: Array[ImageFrame] ): GifInfo = {
    val td = ImageFrame.totalDelay(imageFrames)
    val as = new mutable.HashSet[String]()
    val fr = new mutable.ArrayBuilder.ofRef[immutable.Set[String]]()

    // loop through frames, get every signature, and add signatures to lists
    for( i <- imageFrames ) {
      val bi = i.getImage
      val tiling = TriangleTiling( bi, 9, 5 )
      val sigs = tiling.getSignaturesBase64( bi, .05)

      val f = new mutable.HashSet[String]()
      for( s <- sigs ) {
        as.add(s)
        f.add(s)
      }
      fr += f.toSet
    }

    new GifInfo( id, as.toSet, fr.result(), td )
  }
}

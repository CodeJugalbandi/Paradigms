type Row = List[String]
type Rows = List[Row]


// val source = List(
//   List("TIY6M9",  "simon", "$500"),
//   List("TIY6M0",  "jake",  "$3000"),
//   List("07003",  "wake",   "$1000"),
//   List("07006",  "sandra", "$6500"),
//   List("TIY6M9",  "simon", "$800")
// )
//
// val mirror = List(
//   List("07003","$200"  , "wake",    "MT951"),
//   List("TIY6M9","$800", "simon",  "MT950"),
//   List("07006","$6500",  "sandra",  "MT950"),
//   List("TIY6M9","$500", "simon",  "MT952"),
//   List("07003","$800"  , "wake",    "MT951")
// )
def dataFrom(csvFile: String) = 
  (for {
    line <- io.Source.fromFile(csvFile).getLines
  } yield line.split(",").map(_.trim).toList).toList
  
val source = dataFrom("source_final.csv")
val mirror = dataFrom("mirror_final.csv")

def equals(source: Row, sIdx: Int, mirror: Row, mIdx: Int) =
  source(sIdx).equals(mirror(mIdx))
  
def contains(source: Row, sIdx: Int, mirror: Row, mIdx: Int) =
  source(sIdx).contains(mirror(mIdx))
    
def hasValues(source: Row, sIdx: Int, sValue: String, mirror: Row, mIdx: Int, mValue: String) =
  source(sIdx).equals(sValue) && mirror(mIdx).equals(mValue)
  
def matches(source: Rows, mirror: Rows) = {
  val matched : List[(Row, Row)] = for {
    source_row <- source
    mirror_row <- mirror
    if ((equals(source_row, 2, mirror_row, 1) 
    && equals(source_row, 1, mirror_row, 2)
    && hasValues(source_row, 3, "credit", mirror_row, 4, "debit"))
      || contains(source_row, 1, mirror_row, 2))
  } yield (source_row, mirror_row)
  
  val groupedByFirstColumn = matched.groupBy { 
    case(srow, mrow) => srow(0) 
  }
  groupedByFirstColumn.mapValues { tuple => 
    val (left, right) = tuple.unzip
    (left.distinct, right.distinct) 
  }.values.toList

}

def nonMatches(consolidatedMatches: List[(Rows, Rows)], source: Rows, mirror: Rows): (Rows, Rows) = {
  val (sourceMatched, mirrorMatched) = consolidatedMatches.unzip
  val sMatched = sourceMatched.flatten.distinct
  val mMatched = mirrorMatched.flatten.distinct
  (source diff sMatched, mirror diff mMatched)
}

val consolidatedMatches = matches(source, mirror)
println("Matches...")
consolidatedMatches foreach println

val (sourceUnmatched,mirrorUnmatched) = nonMatches(consolidatedMatches, source, mirror)
println("Non Matches....")
println(s"Source Unmatched = $sourceUnmatched")
println(s"Mirror Unmatched = $mirrorUnmatched")

println("Done!")
println("Done!")

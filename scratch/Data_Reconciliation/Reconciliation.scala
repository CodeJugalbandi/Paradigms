type Row = List[String]
type Rows = List[Row]

val source = List(
  List("TIY6M9",	"simon", "$500"),  
  List("TIY6M0",	"jake",  "$3000"),
  List("07003",  "wake",   "$1000"),
  List("07006",  "sandra", "$6500"),
  List("TIY6M9",	"simon", "$800")  
)

val mirror = List(
  List("07003","$200"	, "wake",	  "MT951"),
  List("TIY6M9","$800", "simon",	"MT950"),
  List("07006","$6500",	"sandra",	"MT950"),
  List("TIY6M9","$500", "simon",	"MT952"),
  List("07003","$800"	, "wake",	  "MT951")
)

def matches(source: Rows, mirror: Rows) = {
  val matched : List[(Row, Row)] = for {
    source_row <- source
    mirror_row <- mirror
    if source_row(0) == mirror_row(0)
  } yield (source_row, mirror_row)
  
  def consolidate(acc: List[(Rows, Rows)], records: List[(Row, Row)]): List[(Rows, Rows)] = records match {
    case Nil => acc
    case (s,m)::rest =>
       val filtered = records.filter { case((srow, mrow)) => srow(0) == s(0) && mrow(0) == m(0) }
       val remaining = rest diff filtered
       val (left, right) = filtered.unzip
       consolidate(acc :+ (left.distinct, right.distinct), remaining)
  }
  
  consolidate(Nil, matched)
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

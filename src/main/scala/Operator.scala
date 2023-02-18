class Operator(val symbol: String) {
    def isUnary: Boolean = List("*", "+", "?").contains(symbol)

    def getPrecedence: Int =
        symbol match
            case "|" => 1
            case "." => 2
            case "*" => 3
            case "?" => 3
            case "+" => 3
            case _ => 0

    override def toString: String =
        symbol match
            case "|" => "UNION"
            case "." => "CONCAT"
            case "*" => "STAR"
            case "?" => "MAYBE"
            case "+" => "PLUS"
            case _ => symbol
}

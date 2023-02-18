class Variable(val symbol: String)
{
    override def toString: String =
        symbol match
            case " " => "' '"
            case "\n" => "'\n'"
            case "\r" => "'\r'"
            case "\t" => "'\t'"
            case _ => symbol

}

import scala.annotation.unused
import scala.collection.mutable

object Regex
{
    def expand(s: List[Char]): List[Either[Variable,Operator]] =
        val start = s.tail.head.toInt
        val end = s.drop(3).head.toInt
        val variables = Range(start, end+1, 1).map(x => new Variable(x.toChar.toString))
        val partial = variables.foldRight(List[Either[Variable,Operator]]())( (x, acc) => Right(new Operator("|"))::Left(x)::acc ).tail
        Right(new Operator("(")) :: (partial ++ List(Right(new Operator(")"))))

    def quotedVariable(s: List[Char]): String = s.foldLeft("")((acc, x) => acc.appended(x))

    def preprocess(s:List[Char]): List[Either[Variable,Operator]] =
        if (s.isEmpty)
            List()
        else
            s.head match
                case '[' => expand(s) ++ preprocess(s.drop(5))
                case '(' => Right(new Operator("(")) :: preprocess(s.tail)
                case ')' => Right(new Operator(")")) :: preprocess(s.tail)
                case 'e' => if s.tail.head == 'p' && s.drop(2).head == 's'
                            then Left(new Variable("eps")) :: preprocess(s.drop(3))
                            else Left(new Variable(s.head.toString)) :: preprocess(s.tail)
                case 'v' => if s.tail.head == 'o' && s.drop(2).head == 'i' && s.drop(3).head == 'd'
                            then Left(new Variable("eps")) :: preprocess(s.drop(3))
                            else Left(new Variable(s.head.toString)) :: preprocess(s.tail)
                case '\'' => val nextQuoteIndex = s.tail.indexOf('\'')
                             val variableText = quotedVariable(s.slice(1, nextQuoteIndex + 1))
                             Left(new Variable(variableText)) :: preprocess(s.drop(nextQuoteIndex + 2))
                case '*' => Right(new Operator("*")) :: preprocess(s.tail)
                case '|' => Right(new Operator("|")) :: preprocess(s.tail)
                case '?' => Right(new Operator("?")) :: preprocess(s.tail)
                case '+' => Right(new Operator("+")) :: preprocess(s.tail)
                case _ => Left(new Variable(s.head.toString)) :: preprocess(s.tail)

    def preprocess2(tokens: List[Either[Variable,Operator]]): List[Either[Variable,Operator]] =
        tokens match
            case x::y::next =>
                x match
                    case Left(value1) =>
                        y match
                            case Left(value2) => x :: Right(new Operator(".")) :: preprocess2(y::next)
                            case Right(value2) => if value2.symbol == "(" then x :: Right(new Operator(".")) :: preprocess2(y::next) else x :: preprocess2(y::next)
                    case Right(value1) =>
                        y match
                            case Left(value2) => if value1.isUnary || value1.symbol == ")" then x :: Right(new Operator(".")) :: preprocess2(y::next) else x :: preprocess2(y::next)
                            case Right(value2) =>
                                if value1.symbol == ")" && value2.symbol == "(" then
                                    x ::  Right(new Operator(".")) :: preprocess2(y::next)
                                else
                                    if value1.isUnary && value2.symbol == "(" then
                                        x ::  Right(new Operator(".")) :: preprocess2(y::next)
                                    else
                                        x :: preprocess2(y::next)
            case x::next => x :: preprocess2(next)
            case Nil => Nil

    def expressionTree(tokens: List[Either[Variable,Operator]], opStack: mutable.Stack[Operator], nodeStack: mutable.Stack[TreeNode]): TreeNode =
        tokens match
            case head::next =>
                head match
                    case Left(value) =>
                        nodeStack.push(TreeNode(Left(value), null, null))
                        expressionTree(next, opStack, nodeStack)
                    case Right(value) =>
                        value.symbol match
                            case "(" =>
                                opStack.push(value)
                                expressionTree(next, opStack, nodeStack)
                            case ")" =>
                                opStack.size match
                                    case 0 => expressionTree(next, opStack, nodeStack)
                                    case _ =>
                                        opStack.top.symbol match
                                            case "(" =>
                                                opStack.pop()
                                                expressionTree(next, opStack, nodeStack)
                                            case _ =>
                                                val op = opStack.pop()
                                                val r = nodeStack.pop()
                                                if op.isUnary then
                                                    nodeStack.push(TreeNode(Right(op), null, r))
                                                else
                                                    val l = nodeStack.pop()
                                                    nodeStack.push(TreeNode(Right(op), l, r))
                                                expressionTree(head::next, opStack, nodeStack)
                            case _ =>
                                opStack.size match
                                    case 0 =>
                                        opStack.push(value)
                                        expressionTree(next, opStack, nodeStack)
                                    case _ =>
                                        if opStack.top.getPrecedence >= value.getPrecedence then
                                            val op = opStack.pop()
                                            val r = nodeStack.pop()
                                            if op.isUnary then
                                                nodeStack.push(TreeNode(Right(op), null, r))
                                            else
                                                val l = nodeStack.pop()
                                                nodeStack.push(TreeNode(Right(op), l, r))
                                            expressionTree(head :: next, opStack, nodeStack)
                                        else
                                            opStack.push(value)
                                            expressionTree(next, opStack, nodeStack)
            case Nil => nodeStack.pop()

    def RSD(node: TreeNode): String =
        node match
            case TreeNode(Left(info), left, right) => info.toString ++ " " ++ RSD(left) ++ RSD(right)
            case TreeNode(Right(info), left, right) => info.toString ++ " " ++ RSD(left) ++ RSD(right)
            case null => ""

    def toPrenex(str: String): String =
        val finalList = (Right(new Operator("(")) :: preprocess2(preprocess(str.toList))) ++ List(Right(new Operator(")")))
        val exprTree = expressionTree(finalList, mutable.Stack(), mutable.Stack())
        val prenex = RSD(exprTree)
        prenex.slice(0, prenex.length - 1)
}
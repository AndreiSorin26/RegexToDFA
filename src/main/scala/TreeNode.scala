class TreeNode(val info: Either[Variable, Operator], val leftNode: TreeNode, val rightNode: TreeNode)
{
    override def toString: String =
        info match
            case Left(value) => value.toString
            case Right(value) => value.toString
}

object TreeNode
{
    def apply(_info: Either[Variable, Operator], _left: TreeNode, _right: TreeNode): TreeNode =
        new TreeNode(_info, _left, _right)

    def unapply(node: TreeNode): Option[(Either[Variable, Operator], TreeNode, TreeNode)] =
        Some((node.info, node.leftNode, node.rightNode))   
}

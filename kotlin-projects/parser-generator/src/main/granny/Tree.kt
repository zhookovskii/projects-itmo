import java.io.File

class Tree(val node: String) {

    companion object {
        const val TAB = '\t'
        const val SEP = '\n'
        const val ARROW = " -> "
        const val LABEL_OPEN = " [label = \""
        const val LABEL_CLOSE = "\"]\n"
    }

	var result: Double = 0.0

    var children: List<Tree> = listOf()

    private var idx = 0
    private val svgBuilder = StringBuilder()

    constructor(node: String, vararg children: Tree) : this(node) {
        this.children = children.asList()
    }

    fun toSvg(filename: String) {
        svgBuilder.append("digraph {")
        toSvgRecursive(this,0, -1)
        svgBuilder.append("}")
        File(filename).printWriter().use {
            it.println(svgBuilder.toString())
        }
    }

    private fun toSvgRecursive(tree: Tree, cur: Int, prev: Int) {
        svgBuilder.append(TAB)
            .append(cur)
            .append(LABEL_OPEN)
            .append(tree.node)
            .append(LABEL_CLOSE)
        if (prev != -1) {
            svgBuilder.append(TAB)
                .append(prev)
                .append(ARROW)
                .append(cur)
                .append(SEP)
        }
        idx++
        tree.children.forEach {
            idx++
            toSvgRecursive(it, idx, cur)
        }
    }

    override fun equals(other: Any?): Boolean {
        if (other is Tree) {
            return node == other.node && children == other.children
        }
        return false
    }

    override fun hashCode(): Int {
        var result = node.hashCode()
        result = 31 * result + children.hashCode()
        return result
    }

}
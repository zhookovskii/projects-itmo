import java.io.File

class GrannyUtils {
    companion object {
        const val TOKEN_HEADER = "sealed interface GrannyToken\n\n"
        private const val ANALYZER_HEADER_FILE = "./src/main/utils/LexicalAnalyzer_header.txt"
        private const val TREE_SOURCE_FILE = "./src/main/utils/Tree_source.txt"
        private const val PARSER_HEADER_FILE = "./src/main/utils/Parser_header.txt"

        private fun readFile(filename: String) = File(filename).readLines().joinToString("\n")

        fun getAnalyzerHeader(): String = readFile(ANALYZER_HEADER_FILE)

        fun getParserHeader(): String = readFile(PARSER_HEADER_FILE)

        fun treeSource(): String = readFile(TREE_SOURCE_FILE)

        fun getParseFunction(initial: String) = "\tfun parse(inS: InputStream): Tree {" +
                "\n\t\tlex = LexicalAnalyzer(inS)" +
                "\n\t\tlex.nextToken()" +
                "\n\t\treturn $initial()" +
                "\n\t}\n\n"
    }
}